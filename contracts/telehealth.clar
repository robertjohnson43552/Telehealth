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