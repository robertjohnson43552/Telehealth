(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-NOT-FOUND (err u101))
(define-constant ERR-INVALID-AMOUNT (err u102))
(define-constant ERR-ALREADY-REGISTERED (err u103))
(define-constant ERR-INVALID-RATING (err u104))
(define-constant ERR-INSUFFICIENT-REPUTATION (err u205))

(define-constant PLATFORM-FEE-RATE u5)
(define-constant GOLD-TIER-THRESHOLD u1000)
(define-constant PLATINUM-TIER-THRESHOLD u2500)
(define-constant GOLD-DISCOUNT u20)
(define-constant PLATINUM-DISCOUNT u40)

(define-data-var contract-owner principal tx-sender)

(define-map doctor-reputation
  { doctor-id: principal }
  {
    reputation-score: uint,
    tier: (string-ascii 10),
    total-earnings: uint,
    total-consultations: uint,
    last-tier-update: uint,
    platform-fee-discount: uint
  }
)

(define-map reputation-milestones
  { doctor-id: principal }
  {
    first-hundred: bool,
    excellence-streak: uint,
    patient-favorite: bool,
    emergency-hero: bool
  }
)

(define-map doctor-fee-history
  { doctor-id: principal, block-height: uint }
  { consultation-fee: uint, platform-fee: uint }
)

(define-public (initialize-doctor-reputation (doctor-id principal))
  (let ((existing-rep (map-get? doctor-reputation {doctor-id: doctor-id})))
    (asserts! (is-none existing-rep) ERR-ALREADY-REGISTERED)
    (map-set doctor-reputation
      {doctor-id: doctor-id}
      {
        reputation-score: u0,
        tier: "BRONZE",
        total-earnings: u0,
        total-consultations: u0,
        last-tier-update: stacks-block-height,
        platform-fee-discount: u0
      })
    (ok (map-set reputation-milestones
      {doctor-id: doctor-id}
      {
        first-hundred: false,
        excellence-streak: u0,
        patient-favorite: false,
        emergency-hero: false
      }))))

(define-public (update-reputation-score (doctor-id principal) (consultation-rating uint) (consultation-fee uint))
  (let 
    (
      (current-rep (unwrap! (map-get? doctor-reputation {doctor-id: doctor-id}) ERR-NOT-FOUND))
      (milestones (unwrap! (map-get? reputation-milestones {doctor-id: doctor-id}) ERR-NOT-FOUND))
      (new-consultations (+ (get total-consultations current-rep) u1))
      (rating-bonus (if (>= consultation-rating u5) u50 (if (>= consultation-rating u4) u25 u10)))
      (new-score (+ (get reputation-score current-rep) rating-bonus))
      (new-earnings (+ (get total-earnings current-rep) consultation-fee))
    )
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
    (let 
      (
        (updated-tier (determine-tier new-score))
        (fee-discount (calculate-fee-discount updated-tier))
        (updated-milestones (check-milestones milestones new-consultations consultation-rating))
      )
      (map-set doctor-reputation
        {doctor-id: doctor-id}
        {
          reputation-score: new-score,
          tier: updated-tier,
          total-earnings: new-earnings,
          total-consultations: new-consultations,
          last-tier-update: stacks-block-height,
          platform-fee-discount: fee-discount
        })
      (ok (map-set reputation-milestones
        {doctor-id: doctor-id}
        updated-milestones)))))

(define-public (calculate-doctor-fees (doctor-id principal) (base-fee uint))
  (let 
    (
      (doctor-rep (unwrap! (map-get? doctor-reputation {doctor-id: doctor-id}) ERR-NOT-FOUND))
      (platform-fee (/ (* base-fee PLATFORM-FEE-RATE) u100))
      (discount (get platform-fee-discount doctor-rep))
      (discounted-fee (- platform-fee (/ (* platform-fee discount) u100)))
      (doctor-fee-amount (- base-fee discounted-fee))
    )
    (map-set doctor-fee-history
      {doctor-id: doctor-id, block-height: stacks-block-height}
      {consultation-fee: doctor-fee-amount, platform-fee: discounted-fee})
    (ok {doctor-fee: doctor-fee-amount, platform-fee: discounted-fee})))

(define-public (award-milestone-bonus (doctor-id principal) (milestone-type (string-ascii 20)))
  (let 
    (
      (current-rep (unwrap! (map-get? doctor-reputation {doctor-id: doctor-id}) ERR-NOT-FOUND))
      (milestones (unwrap! (map-get? reputation-milestones {doctor-id: doctor-id}) ERR-NOT-FOUND))
      (bonus-points (if (is-eq milestone-type "FIRST-HUNDRED") u100
                      (if (is-eq milestone-type "EXCELLENCE-STREAK") u200
                        (if (is-eq milestone-type "PATIENT-FAVORITE") u150
                          (if (is-eq milestone-type "EMERGENCY-HERO") u300 u0)))))
      (new-score (+ (get reputation-score current-rep) bonus-points))
    )
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
    (asserts! (> bonus-points u0) ERR-INVALID-AMOUNT)
    (ok (map-set doctor-reputation
      {doctor-id: doctor-id}
      (merge current-rep 
        {
          reputation-score: new-score,
          tier: (determine-tier new-score),
          platform-fee-discount: (calculate-fee-discount (determine-tier new-score))
        })))))

(define-private (determine-tier (score uint))
  (if (>= score PLATINUM-TIER-THRESHOLD)
    "PLATINUM"
    (if (>= score GOLD-TIER-THRESHOLD)
      "GOLD"
      "BRONZE")))

(define-private (calculate-fee-discount (tier (string-ascii 10)))
  (if (is-eq tier "PLATINUM")
    PLATINUM-DISCOUNT
    (if (is-eq tier "GOLD")
      GOLD-DISCOUNT
      u0)))

(define-private (check-milestones (current-milestones { first-hundred: bool, excellence-streak: uint, patient-favorite: bool, emergency-hero: bool }) (consultation-count uint) (rating uint))
  (let 
    (
      (first-hundred-check (or (get first-hundred current-milestones) (>= consultation-count u100)))
      (streak-update (if (>= rating u5) (+ (get excellence-streak current-milestones) u1) u0))
      (patient-favorite-check (or (get patient-favorite current-milestones) (>= streak-update u10)))
    )
    {
      first-hundred: first-hundred-check,
      excellence-streak: streak-update,
      patient-favorite: patient-favorite-check,
      emergency-hero: (get emergency-hero current-milestones)
    }))

(define-public (get-doctor-reputation (doctor-id principal))
  (ok (map-get? doctor-reputation {doctor-id: doctor-id})))

(define-public (get-doctor-milestones (doctor-id principal))
  (ok (map-get? reputation-milestones {doctor-id: doctor-id})))

(define-public (get-doctor-earnings-history (doctor-id principal) (block-number uint))
  (ok (map-get? doctor-fee-history {doctor-id: doctor-id, block-height: block-number})))

(define-read-only (get-tier-requirements)
  (ok {
    bronze: u0,
    gold: GOLD-TIER-THRESHOLD,
    platinum: PLATINUM-TIER-THRESHOLD,
    gold-discount: GOLD-DISCOUNT,
    platinum-discount: PLATINUM-DISCOUNT
  }))

(define-read-only (calculate-reputation-bonus (rating uint) (consultation-count uint))
  (let 
    (
      (rating-bonus (if (>= rating u5) u50 (if (>= rating u4) u25 u10)))
      (milestone-bonus (if (is-eq consultation-count u100) u100 u0))
    )
    (ok (+ rating-bonus milestone-bonus))))

(define-read-only (get-platform-stats)
  (ok {
    platform-fee-rate: PLATFORM-FEE-RATE,
    total-tiers: u3,
    max-discount: PLATINUM-DISCOUNT
  }))

;; Dynamic Availability & Pricing System
(define-constant ERR-SLOT-UNAVAILABLE (err u210))
(define-constant ERR-INVALID-TIME-SLOT (err u211))
(define-constant ERR-BOOKING-CONFLICT (err u212))
(define-constant ERR-INSUFFICIENT-PAYMENT (err u213))
(define-constant ERR-INVALID-STATUS (err u214))

(define-constant BASE-CONSULTATION-FEE u10000000)
(define-constant PREMIUM-TIME-MULTIPLIER u150) ;; 50% premium for peak hours
(define-constant SURGE-MULTIPLIER-CAP u300) ;; Max 3x surge pricing
(define-constant SLOT-DURATION-BLOCKS u6) ;; ~1 hour slots
(define-constant MAX-DAILY-SLOTS u12) ;; 12 slots per day maximum

;; Doctor availability calendar - tracks time slots
(define-map doctor-availability
  { doctor-id: principal, slot-id: uint }
  {
    start-block: uint,
    end-block: uint,
    base-price: uint,
    current-price: uint,
    is-premium: bool,
    is-booked: bool,
    booking-deadline: uint,
    demand-level: uint
  }
)

;; Daily availability summary for efficient querying
(define-map daily-availability
  { doctor-id: principal, day-block: uint }
  {
    total-slots: uint,
    booked-slots: uint,
    premium-slots: uint,
    average-price: uint,
    peak-demand: uint
  }
)

;; Surge pricing tracker for real-time demand
(define-map demand-tracker
  { specialty: (string-ascii 50), time-block: uint }
  {
    total-requests: uint,
    available-doctors: uint,
    surge-multiplier: uint,
    last-updated: uint
  }
)

;; Consultation bookings with time slots
(define-map slot-bookings
  { booking-id: uint }
  {
    doctor-id: principal,
    patient-id: principal,
    slot-id: uint,
    booking-time: uint,
    final-price: uint,
    payment-status: (string-ascii 20),
    confirmation-status: (string-ascii 20)
  }
)

(define-data-var booking-counter uint u0)

;; Create availability slots for a doctor
(define-public (create-availability-slots (slot-start uint) (num-slots uint) (base-price uint) (is-premium bool))
  (let 
    (
      (doctor-rep (unwrap! (map-get? doctor-reputation {doctor-id: tx-sender}) ERR-NOT-FOUND))
      (day-block (/ slot-start u144)) ;; Approximate day grouping
    )
    (asserts! (<= num-slots MAX-DAILY-SLOTS) ERR-INVALID-AMOUNT)
    (asserts! (>= base-price (/ BASE-CONSULTATION-FEE u2)) ERR-INVALID-AMOUNT)
    (create-single-slot tx-sender slot-start base-price is-premium u0)
    (if (> num-slots u1)
      (create-single-slot tx-sender (+ slot-start SLOT-DURATION-BLOCKS) base-price is-premium u1)
      true)
    (if (> num-slots u2)
      (create-single-slot tx-sender (+ slot-start (* u2 SLOT-DURATION-BLOCKS)) base-price is-premium u2)
      true)
    (if (> num-slots u3)
      (create-single-slot tx-sender (+ slot-start (* u3 SLOT-DURATION-BLOCKS)) base-price is-premium u3)
      true)
    (update-daily-summary tx-sender day-block num-slots)
    (ok num-slots)))

;; Helper function to create a single slot
(define-private (create-single-slot (doctor-id principal) (start-block uint) (price uint) (premium bool) (index uint))
  (let 
    (
      (slot-id (+ (* start-block u1000) index))
      (slot-end (+ start-block SLOT-DURATION-BLOCKS))
      (adjusted-price (if premium (/ (* price PREMIUM-TIME-MULTIPLIER) u100) price))
    )
    (map-set doctor-availability
      {doctor-id: doctor-id, slot-id: slot-id}
      {
        start-block: start-block,
        end-block: slot-end,
        base-price: price,
        current-price: adjusted-price,
        is-premium: premium,
        is-booked: false,
        booking-deadline: (- start-block u12), ;; 2 hours before
        demand-level: u1
      })
    true))

;; Update daily availability summary
(define-private (update-daily-summary (doctor-id principal) (day-block uint) (new-slots uint))
  (let 
    (
      (current-summary (default-to 
        {total-slots: u0, booked-slots: u0, premium-slots: u0, average-price: BASE-CONSULTATION-FEE, peak-demand: u1}
        (map-get? daily-availability {doctor-id: doctor-id, day-block: day-block})))
    )
    (map-set daily-availability
      {doctor-id: doctor-id, day-block: day-block}
      (merge current-summary {total-slots: (+ (get total-slots current-summary) new-slots)}))))

;; Book a specific time slot with dynamic pricing
(define-public (book-time-slot (doctor-id principal) (slot-id uint) (specialty (string-ascii 50)))
  (let 
    (
      (slot-info (unwrap! (map-get? doctor-availability {doctor-id: doctor-id, slot-id: slot-id}) ERR-NOT-FOUND))
      (booking-id (+ (var-get booking-counter) u1))
      (current-block stacks-block-height)
      (surge-price (calculate-surge-pricing specialty (get start-block slot-info)))
      (final-price (/ (* (get current-price slot-info) surge-price) u100))
    )
    (asserts! (not (get is-booked slot-info)) ERR-SLOT-UNAVAILABLE)
    (asserts! (< current-block (get booking-deadline slot-info)) ERR-INVALID-TIME-SLOT)
    (asserts! (< current-block (get start-block slot-info)) ERR-INVALID-TIME-SLOT)
    
    ;; Transfer payment
    (try! (stx-transfer? final-price tx-sender (as-contract tx-sender)))
    
    ;; Update slot as booked
    (map-set doctor-availability
      {doctor-id: doctor-id, slot-id: slot-id}
      (merge slot-info {is-booked: true, demand-level: (+ (get demand-level slot-info) u1)}))
    
    ;; Create booking record
    (map-set slot-bookings
      {booking-id: booking-id}
      {
        doctor-id: doctor-id,
        patient-id: tx-sender,
        slot-id: slot-id,
        booking-time: current-block,
        final-price: final-price,
        payment-status: "PAID",
        confirmation-status: "PENDING"
      })
    
    (var-set booking-counter booking-id)
    (update-demand-tracking specialty (get start-block slot-info))
    (ok booking-id)))

;; Calculate surge pricing based on demand
(define-private (calculate-surge-pricing (specialty (string-ascii 50)) (time-block uint))
  (let 
    (
      (demand-data (default-to 
        {total-requests: u1, available-doctors: u1, surge-multiplier: u100, last-updated: u0}
        (map-get? demand-tracker {specialty: specialty, time-block: time-block})))
      (demand-ratio (if (> (get available-doctors demand-data) u0) 
        (/ (get total-requests demand-data) (get available-doctors demand-data)) u1))
      (surge-multiplier (min (+ u100 (* demand-ratio u25)) SURGE-MULTIPLIER-CAP))
    )
    surge-multiplier))

;; Update demand tracking for surge pricing
(define-private (update-demand-tracking (specialty (string-ascii 50)) (time-block uint))
  (let 
    (
      (current-demand (default-to 
        {total-requests: u0, available-doctors: u1, surge-multiplier: u100, last-updated: u0}
        (map-get? demand-tracker {specialty: specialty, time-block: time-block})))
      (new-requests (+ (get total-requests current-demand) u1))
    )
    (map-set demand-tracker
      {specialty: specialty, time-block: time-block}
      (merge current-demand 
        {
          total-requests: new-requests,
          surge-multiplier: (calculate-surge-pricing specialty time-block),
          last-updated: stacks-block-height
        }))))

;; Confirm consultation completion and release payment
(define-public (confirm-slot-consultation (booking-id uint))
  (let 
    (
      (booking (unwrap! (map-get? slot-bookings {booking-id: booking-id}) ERR-NOT-FOUND))
      (doctor-id (get doctor-id booking))
    )
    (asserts! (is-eq tx-sender doctor-id) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq (get confirmation-status booking) "PENDING") ERR-INVALID-STATUS)
    
    ;; Release payment to doctor
    (try! (as-contract (stx-transfer? (get final-price booking) (as-contract tx-sender) doctor-id)))
    
    ;; Update booking status
    (map-set slot-bookings
      {booking-id: booking-id}
      (merge booking {confirmation-status: "COMPLETED"}))
    
    (ok true)))

;; Cancel booking with refund (if within cancellation window)
(define-public (cancel-slot-booking (booking-id uint))
  (let 
    (
      (booking (unwrap! (map-get? slot-bookings {booking-id: booking-id}) ERR-NOT-FOUND))
      (slot-info (unwrap! (map-get? doctor-availability 
        {doctor-id: (get doctor-id booking), slot-id: (get slot-id booking)}) ERR-NOT-FOUND))
      (cancellation-deadline (- (get start-block slot-info) u24)) ;; 4 hours before
    )
    (asserts! (is-eq tx-sender (get patient-id booking)) ERR-NOT-AUTHORIZED)
    (asserts! (< stacks-block-height cancellation-deadline) ERR-INVALID-TIME-SLOT)
    
    ;; Refund 90% of payment (10% cancellation fee)
    (let ((refund-amount (/ (* (get final-price booking) u90) u100)))
      (try! (as-contract (stx-transfer? refund-amount (as-contract tx-sender) (get patient-id booking))))
    
    ;; Mark slot as available again
    (map-set doctor-availability
      {doctor-id: (get doctor-id booking), slot-id: (get slot-id booking)}
      (merge slot-info {is-booked: false}))
    
    ;; Update booking status
    (map-set slot-bookings
      {booking-id: booking-id}
      (merge booking {confirmation-status: "CANCELLED"}))
    
    (ok refund-amount))))

;; Get available slots for a doctor on a specific day
(define-read-only (get-doctor-availability (doctor-id principal) (day-block uint))
  (ok (map-get? daily-availability {doctor-id: doctor-id, day-block: day-block})))

;; Get specific slot details
(define-read-only (get-slot-details (doctor-id principal) (slot-id uint))
  (ok (map-get? doctor-availability {doctor-id: doctor-id, slot-id: slot-id})))

;; Get booking information
(define-read-only (get-booking-details (booking-id uint))
  (ok (map-get? slot-bookings {booking-id: booking-id})))

;; Get current surge pricing for specialty and time
(define-read-only (get-surge-pricing (specialty (string-ascii 50)) (time-block uint))
  (ok (map-get? demand-tracker {specialty: specialty, time-block: time-block})))

;; Helper function to get minimum value
(define-private (min (a uint) (b uint))
  (if (< a b) a b))



