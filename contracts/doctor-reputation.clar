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
