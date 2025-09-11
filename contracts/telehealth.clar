(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-NOT-FOUND (err u101))
(define-constant ERR-INVALID-AMOUNT (err u102))
(define-constant ERR-ALREADY-REGISTERED (err u103))
(define-constant CONSULTATION-FEE u10000000)

(define-data-var contract-owner principal tx-sender)

(define-map doctors 
  { doctor-id: principal }
  {
    name: (string-ascii 50),
    specialty: (string-ascii 50),
    verified: bool,
    consultation-count: uint
  }
)

(define-map patients
  { patient-id: principal }
  {
    name: (string-ascii 50),
    consultations: uint,
    last-consultation: uint
  }
)

(define-map consultations
  { consultation-id: uint }
  {
    doctor: principal,
    patient: principal,
    timestamp: uint,
    status: (string-ascii 20),
    fee: uint
  }
)

(define-data-var consultation-counter uint u0)

(define-public (register-doctor (name (string-ascii 50)) (specialty (string-ascii 50)))
  (let ((doctor-exists (get verified (map-get? doctors {doctor-id: tx-sender}))))
    (asserts! (is-none doctor-exists) ERR-ALREADY-REGISTERED)
    (ok (map-set doctors
      { doctor-id: tx-sender }
      {
        name: name,
        specialty: specialty,
        verified: false,
        consultation-count: u0
      }))))

(define-public (verify-doctor (doctor-id principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
    (match (map-get? doctors {doctor-id: doctor-id})
      doctor (ok (map-set doctors
        {doctor-id: doctor-id}
        (merge doctor {verified: true})))
      ERR-NOT-FOUND)))

(define-public (register-patient (name (string-ascii 50)))
  (ok (map-set patients
    {patient-id: tx-sender}
    {
      name: name,
      consultations: u0,
      last-consultation: u0
    })))

(define-public (request-consultation (doctor-id principal))
  (let 
    (
      (doctor (unwrap! (map-get? doctors {doctor-id: doctor-id}) ERR-NOT-FOUND))
      (consultation-id (+ (var-get consultation-counter) u1))
    )
    (asserts! (get verified doctor) ERR-NOT-AUTHORIZED)
    (try! (stx-transfer? CONSULTATION-FEE tx-sender (as-contract tx-sender)))
    (var-set consultation-counter consultation-id)
    (ok (map-set consultations
      {consultation-id: consultation-id}
      {
        doctor: doctor-id,
        patient: tx-sender,
        timestamp: stacks-block-height,
        status: "PENDING",
        fee: CONSULTATION-FEE
      }))))

(define-public (accept-consultation (consultation-id uint))
  (let ((consultation (unwrap! (map-get? consultations {consultation-id: consultation-id}) ERR-NOT-FOUND)))
    (asserts! (is-eq (get doctor consultation) tx-sender) ERR-NOT-AUTHORIZED)
    (ok (map-set consultations
      {consultation-id: consultation-id}
      (merge consultation {status: "ACCEPTED"})))))

(define-public (complete-consultation (consultation-id uint))
  (let 
    (
      (consultation (unwrap! (map-get? consultations {consultation-id: consultation-id}) ERR-NOT-FOUND))
      (doctor (unwrap! (map-get? doctors {doctor-id: (get doctor consultation)}) ERR-NOT-FOUND))
    )
    (asserts! (is-eq (get doctor consultation) tx-sender) ERR-NOT-AUTHORIZED)
    (try! (as-contract (stx-transfer? CONSULTATION-FEE (as-contract tx-sender) (get doctor consultation))))
    (ok (map-set consultations
      {consultation-id: consultation-id}
      (merge consultation {status: "COMPLETED"})))))

(define-read-only (get-doctor (doctor-id principal))
  (ok (map-get? doctors {doctor-id: doctor-id})))

(define-read-only (get-patient (patient-id principal))
  (ok (map-get? patients {patient-id: patient-id})))

(define-read-only (get-consultation (consultation-id uint))
  (ok (map-get? consultations {consultation-id: consultation-id})))


  (define-constant ERR-INVALID-RATING (err u104))
(define-constant ERR-NOT-CONSULTED (err u105))

(define-map doctor-ratings
  { doctor-id: principal }
  {
    total-rating: uint,
    rating-count: uint,
    average-rating: uint
  }
)

(define-map patient-ratings
  { patient-id: principal, doctor-id: principal }
  { has-rated: bool }
)

(define-public (rate-doctor (doctor-id principal) (rating uint))
  (let 
    (
      (consultation (unwrap! (map-get? consultations {consultation-id: (var-get consultation-counter)}) ERR-NOT-FOUND))
      (current-ratings (default-to {total-rating: u0, rating-count: u0, average-rating: u0} (map-get? doctor-ratings {doctor-id: doctor-id})))
      (patient-rating (default-to {has-rated: false} (map-get? patient-ratings {patient-id: tx-sender, doctor-id: doctor-id})))
    )
    (asserts! (and (>= rating u1) (<= rating u5)) ERR-INVALID-RATING)
    (asserts! (is-eq (get status consultation) "COMPLETED") ERR-NOT-CONSULTED)
    (asserts! (not (get has-rated patient-rating)) ERR-ALREADY-REGISTERED)
    (ok (begin
      (map-set doctor-ratings
        {doctor-id: doctor-id}
        {
          total-rating: (+ (get total-rating current-ratings) rating),
          rating-count: (+ (get rating-count current-ratings) u1),
          average-rating: (/ (+ (get total-rating current-ratings) rating) (+ (get rating-count current-ratings) u1))
        })
      (map-set patient-ratings
        {patient-id: tx-sender, doctor-id: doctor-id}
        {has-rated: true})))))



(define-constant REFUND-WINDOW-BLOCKS u144)
(define-constant ERR-REFUND-EXPIRED (err u106))
(define-constant ERR-INVALID-STATUS (err u107))

(define-public (cancel-consultation (consultation-id uint))
  (let 
    (
      (consultation (unwrap! (map-get? consultations {consultation-id: consultation-id}) ERR-NOT-FOUND))
      (current-height stacks-block-height)
    )
    (asserts! (is-eq (get patient consultation) tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq (get status consultation) "PENDING") ERR-INVALID-STATUS)
    (asserts! (<= (- current-height (get timestamp consultation)) REFUND-WINDOW-BLOCKS) ERR-REFUND-EXPIRED)
    (try! (as-contract (stx-transfer? CONSULTATION-FEE (as-contract tx-sender) (get patient consultation))))
    (ok (map-set consultations
      {consultation-id: consultation-id}
      (merge consultation {status: "CANCELLED"})))))


(define-constant ERR-NO-EMERGENCY-DOCTORS (err u108))
(define-constant ERR-NOT-EMERGENCY-QUALIFIED (err u109))
(define-constant EMERGENCY-CONSULTATION-FEE u25000000)
(define-constant EMERGENCY-BONUS u5000000)

(define-map emergency-doctors
  { doctor-id: principal }
  { 
    available: bool,
    emergency-count: uint,
    last-emergency: uint
  }
)

(define-map emergency-consultations
  { emergency-id: uint }
  {
    doctor: (optional principal),
    patient: principal,
    timestamp: uint,
    status: (string-ascii 20),
    severity: (string-ascii 10),
    fee: uint,
    response-time: uint
  }
)

(define-data-var emergency-counter uint u0)

(define-public (register-emergency-doctor)
  (let ((doctor (unwrap! (map-get? doctors {doctor-id: tx-sender}) ERR-NOT-FOUND)))
    (asserts! (get verified doctor) ERR-NOT-AUTHORIZED)
    (asserts! (>= (get consultation-count doctor) u10) (err u204))
    (ok (map-set emergency-doctors
      {doctor-id: tx-sender}
      {
        available: true,
        emergency-count: u0,
        last-emergency: u0
      }))))

(define-public (request-emergency-consultation (severity (string-ascii 10)))
  (let 
    (
      (emergency-id (+ (var-get emergency-counter) u1))
      (patient (unwrap! (map-get? patients {patient-id: tx-sender}) ERR-NOT-FOUND))
    )
    (try! (stx-transfer? EMERGENCY-CONSULTATION-FEE tx-sender (as-contract tx-sender)))
    (var-set emergency-counter emergency-id)
    (ok (map-set emergency-consultations
      {emergency-id: emergency-id}
      {
        doctor: none,
        patient: tx-sender,
        timestamp: stacks-block-height,
        status: "URGENT",
        severity: severity,
        fee: EMERGENCY-CONSULTATION-FEE,
        response-time: u0
      }))))

(define-public (accept-emergency-consultation (emergency-id uint))
  (let 
    (
      (emergency (unwrap! (map-get? emergency-consultations {emergency-id: emergency-id}) ERR-NOT-FOUND))
      (emergency-doctor (unwrap! (map-get? emergency-doctors {doctor-id: tx-sender}) ERR-NOT-FOUND))
      (response-time (- stacks-block-height (get timestamp emergency)))
    )
    (asserts! (get available emergency-doctor) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq (get status emergency) "URGENT") ERR-INVALID-STATUS)
    (try! (as-contract (stx-transfer? (get fee emergency) (as-contract tx-sender) tx-sender)))
    (try! (as-contract (stx-transfer? EMERGENCY-BONUS (as-contract tx-sender) tx-sender)))
    (map-set emergency-doctors
      {doctor-id: tx-sender}
      (merge emergency-doctor 
        {
          available: false,
          emergency-count: (+ (get emergency-count emergency-doctor) u1),
          last-emergency: stacks-block-height
        }))
    (ok (map-set emergency-consultations
      {emergency-id: emergency-id}
      (merge emergency 
        {
          doctor: (some tx-sender),
          status: "IN-PROGRESS",
          response-time: response-time
        })))))

(define-public (complete-emergency-consultation (emergency-id uint))
  (let 
    (
      (emergency (unwrap! (map-get? emergency-consultations {emergency-id: emergency-id}) ERR-NOT-FOUND))
      (doctor-id (unwrap! (get doctor emergency) ERR-NOT-FOUND))
    )
    (asserts! (is-eq doctor-id tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq (get status emergency) "IN-PROGRESS") ERR-INVALID-STATUS)
    (map-set emergency-doctors
      {doctor-id: tx-sender}
      (merge (unwrap! (map-get? emergency-doctors {doctor-id: tx-sender}) ERR-NOT-FOUND)
        {available: true}))
    (ok (map-set emergency-consultations
      {emergency-id: emergency-id}
      (merge emergency {status: "RESOLVED"})))))

(define-public (set-emergency-availability (available bool))
  (let ((emergency-doctor (unwrap! (map-get? emergency-doctors {doctor-id: tx-sender}) ERR-NOT-FOUND)))
    (ok (map-set emergency-doctors
      {doctor-id: tx-sender}
      (merge emergency-doctor {available: available})))))

(define-read-only (get-emergency-consultation (emergency-id uint))
  (ok (map-get? emergency-consultations {emergency-id: emergency-id})))

(define-read-only (get-emergency-doctor-status (doctor-id principal))
  (ok (map-get? emergency-doctors {doctor-id: doctor-id})))

(define-read-only (get-available-emergency-doctors)
  (ok (var-get emergency-counter)))