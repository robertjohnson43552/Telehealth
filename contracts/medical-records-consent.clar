;; Medical Records Consent Management System
;; Secure, privacy-focused consent system for telehealth medical data sharing
;; Patients control access to medical history during consultations

(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u300))
(define-constant ERR_CONSENT_NOT_FOUND (err u301))
(define-constant ERR_CONSENT_EXPIRED (err u302))
(define-constant ERR_INVALID_CONSULTATION (err u303))
(define-constant ERR_ACCESS_DENIED (err u304))
(define-constant ERR_CONSENT_EXISTS (err u305))

;; Consent duration constants (in blocks)
(define-constant STANDARD_CONSENT_DURATION u144)  ;; ~24 hours
(define-constant EMERGENCY_CONSENT_DURATION u72)  ;; ~12 hours
(define-constant EXTENDED_CONSENT_DURATION u288)  ;; ~48 hours

;; Medical data categories 
(define-constant CATEGORY_ALLERGIES u1)
(define-constant CATEGORY_MEDICATIONS u2)
(define-constant CATEGORY_MEDICAL_HISTORY u4)
(define-constant CATEGORY_TEST_RESULTS u8)
(define-constant CATEGORY_VITAL_SIGNS u16)
(define-constant CATEGORY_EMERGENCY_CONTACTS u32)

(define-data-var consent-counter uint u0)
(define-data-var audit-counter uint u0)

;; Patient consent records
(define-map patient-consents
  { patient: principal, consent-id: uint }
  {
    doctor: principal,
    consultation-id: (optional uint),
    granted-categories: uint, ;; Bitfield for data categories
    access-level: uint,       ;; 1: read-only, 2: read-write, 3: emergency
    start-time: uint,
    expiry-time: uint,
    is-active: bool,
    emergency-override: bool,
    notes: (string-ascii 100)
  }
)

;; Active access permissions
(define-map active-permissions
  { doctor: principal, patient: principal }
  {
    consent-id: uint,
    last-accessed: uint,
    access-count: uint,
    categories-accessed: uint,
    session-active: bool
  }
)

;; Medical data access audit trail
(define-map access-audit-log
  { audit-id: uint }
  {
    doctor: principal,
    patient: principal,
    consent-id: uint,
    data-category: uint,
    access-type: uint, ;; 1: view, 2: update, 3: download
    access-time: uint,
    ip-hash: (optional (buff 20)),
    purpose: (string-ascii 100)
  }
)

;; Patient privacy preferences
(define-map patient-privacy-settings
  { patient: principal }
  {
    default-categories: uint,
    require-explicit-consent: bool,
    max-consent-duration: uint,
    allow-emergency-override: bool,
    audit-notifications: bool
  }
)

;; Grant consent for medical data access
(define-public (grant-medical-consent 
  (doctor principal) 
  (consultation-id (optional uint))
  (data-categories uint) 
  (access-level uint)
  (duration-blocks uint)
  (notes (string-ascii 100)))
  (let (
    (consent-id (+ (var-get consent-counter) u1))
    (current-time stacks-block-height)
    (expiry-time (+ current-time duration-blocks))
    (privacy-settings (default-to 
      { default-categories: u31, require-explicit-consent: true, max-consent-duration: EXTENDED_CONSENT_DURATION, allow-emergency-override: true, audit-notifications: true }
      (map-get? patient-privacy-settings { patient: tx-sender })))
  )
    ;; Validate inputs
    (asserts! (> data-categories u0) ERR_UNAUTHORIZED)
    (asserts! (<= access-level u3) ERR_UNAUTHORIZED)
    (asserts! (<= duration-blocks (get max-consent-duration privacy-settings)) ERR_UNAUTHORIZED)
    
    ;; Check if doctor is verified (integration with main telehealth contract)
    (let (
      (doctor-data (unwrap! (contract-call? .telehealth get-doctor doctor) ERR_UNAUTHORIZED))
    )
      (match doctor-data
        doc-info (begin (asserts! (get verified doc-info) ERR_UNAUTHORIZED) true)
        (asserts! false ERR_UNAUTHORIZED)
      )
    )
    
    ;; Create consent record
    (map-set patient-consents { patient: tx-sender, consent-id: consent-id } {
      doctor: doctor,
      consultation-id: consultation-id,
      granted-categories: data-categories,
      access-level: access-level,
      start-time: current-time,
      expiry-time: expiry-time,
      is-active: true,
      emergency-override: false,
      notes: notes
    })
    
    ;; Activate permissions
    (map-set active-permissions { doctor: doctor, patient: tx-sender } {
      consent-id: consent-id,
      last-accessed: u0,
      access-count: u0,
      categories-accessed: u0,
      session-active: true
    })
    
    (var-set consent-counter consent-id)
    (ok consent-id)
  )
)

;; Access medical data with consent verification
(define-public (access-medical-data 
  (patient principal) 
  (data-category uint) 
  (access-type uint)
  (purpose (string-ascii 100)))
  (let (
    (permission (unwrap! (map-get? active-permissions { doctor: tx-sender, patient: patient }) ERR_ACCESS_DENIED))
    (consent (unwrap! (map-get? patient-consents { patient: patient, consent-id: (get consent-id permission) }) ERR_CONSENT_NOT_FOUND))
    (audit-id (+ (var-get audit-counter) u1))
  )
    ;; Verify access permissions
    (asserts! (get is-active consent) ERR_CONSENT_EXPIRED)
    (asserts! (< stacks-block-height (get expiry-time consent)) ERR_CONSENT_EXPIRED)
    (asserts! (get session-active permission) ERR_ACCESS_DENIED)
    (asserts! (> (bit-and (get granted-categories consent) data-category) u0) ERR_ACCESS_DENIED)
    (asserts! (<= access-type (get access-level consent)) ERR_ACCESS_DENIED)
    
    ;; Log access in audit trail
    (map-set access-audit-log { audit-id: audit-id } {
      doctor: tx-sender,
      patient: patient,
      consent-id: (get consent-id permission),
      data-category: data-category,
      access-type: access-type,
      access-time: stacks-block-height,
      ip-hash: none,
      purpose: purpose
    })
    
    ;; Update access tracking
    (map-set active-permissions { doctor: tx-sender, patient: patient }
      (merge permission {
        last-accessed: stacks-block-height,
        access-count: (+ (get access-count permission) u1),
        categories-accessed: (bit-or (get categories-accessed permission) data-category)
      })
    )
    
    (var-set audit-counter audit-id)
    (ok audit-id)
  )
)

;; Revoke consent and terminate access
(define-public (revoke-consent (doctor principal) (consent-id uint))
  (let (
    (consent (unwrap! (map-get? patient-consents { patient: tx-sender, consent-id: consent-id }) ERR_CONSENT_NOT_FOUND))
  )
    (asserts! (is-eq (get doctor consent) doctor) ERR_UNAUTHORIZED)
    (asserts! (get is-active consent) ERR_CONSENT_EXPIRED)
    
    ;; Deactivate consent
    (map-set patient-consents { patient: tx-sender, consent-id: consent-id }
      (merge consent { is-active: false })
    )
    
    ;; End active session
    (match (map-get? active-permissions { doctor: doctor, patient: tx-sender })
      permission (map-set active-permissions { doctor: doctor, patient: tx-sender }
        (merge permission { session-active: false }))
      true
    )
    
    (ok true)
  )
)

;; Set patient privacy preferences
(define-public (set-privacy-preferences 
  (default-categories uint)
  (require-explicit bool)
  (max-duration uint)
  (allow-emergency bool))
  (begin
    (asserts! (<= max-duration EXTENDED_CONSENT_DURATION) ERR_UNAUTHORIZED)
    (asserts! (> default-categories u0) ERR_UNAUTHORIZED)
    
    (map-set patient-privacy-settings { patient: tx-sender } {
      default-categories: default-categories,
      require-explicit-consent: require-explicit,
      max-consent-duration: max-duration,
      allow-emergency-override: allow-emergency,
      audit-notifications: true
    })
    (ok true)
  )
)

;; Emergency override for critical consultations
(define-public (emergency-override-consent 
  (patient principal) 
  (emergency-consultation-id uint)
  (justification (string-ascii 100)))
  (let (
    (privacy-settings (default-to 
      { default-categories: u31, require-explicit-consent: true, max-consent-duration: EXTENDED_CONSENT_DURATION, allow-emergency-override: true, audit-notifications: true }
      (map-get? patient-privacy-settings { patient: patient })))
    (consent-id (+ (var-get consent-counter) u1))
    (audit-id (+ (var-get audit-counter) u1))
  )
    ;; Verify emergency consultation exists and doctor is authorized
    (let (
      (emergency-data (unwrap! (contract-call? .telehealth get-emergency-consultation emergency-consultation-id) ERR_INVALID_CONSULTATION))
    )
      (match emergency-data
        emergency (begin
          (asserts! (is-eq (unwrap! (get doctor emergency) ERR_UNAUTHORIZED) tx-sender) ERR_UNAUTHORIZED)
          (asserts! (get allow-emergency-override privacy-settings) ERR_ACCESS_DENIED)
          
          ;; Create emergency consent
          (map-set patient-consents { patient: patient, consent-id: consent-id } {
            doctor: tx-sender,
            consultation-id: (some emergency-consultation-id),
            granted-categories: u63, ;; All categories for emergency
            access-level: u3,
            start-time: stacks-block-height,
            expiry-time: (+ stacks-block-height EMERGENCY_CONSENT_DURATION),
            is-active: true,
            emergency-override: true,
            notes: justification
          })
          
          ;; Log emergency override
          (map-set access-audit-log { audit-id: audit-id } {
            doctor: tx-sender,
            patient: patient,
            consent-id: consent-id,
            data-category: u999, ;; Special code for emergency override
            access-type: u3,
            access-time: stacks-block-height,
            ip-hash: none,
            purpose: justification
          })
          
          (var-set consent-counter consent-id)
          (var-set audit-counter audit-id)
          (ok consent-id)
        )
        ERR_INVALID_CONSULTATION
      )
    )
  )
)

;; Read-only functions

(define-read-only (get-patient-consent (patient principal) (consent-id uint))
  (map-get? patient-consents { patient: patient, consent-id: consent-id })
)

(define-read-only (get-active-permissions (doctor principal) (patient principal))
  (map-get? active-permissions { doctor: doctor, patient: patient })
)

(define-read-only (get-access-audit (audit-id uint))
  (map-get? access-audit-log { audit-id: audit-id })
)

(define-read-only (get-patient-privacy-settings (patient principal))
  (map-get? patient-privacy-settings { patient: patient })
)

(define-read-only (check-data-access-permission (doctor principal) (patient principal) (data-category uint))
  (match (map-get? active-permissions { doctor: doctor, patient: patient })
    permission 
      (match (map-get? patient-consents { patient: patient, consent-id: (get consent-id permission) })
        consent (ok {
          has-access: (and 
            (get is-active consent)
            (< stacks-block-height (get expiry-time consent))
            (> (bit-and (get granted-categories consent) data-category) u0)
          ),
          expires-at: (get expiry-time consent),
          access-level: (get access-level consent)
        })
        (err u0)
      )
    (err u0)
  )
)

(define-read-only (get-consent-summary (patient principal))
  ;; Returns summary of all active consents for a patient
  (let (
    (privacy-settings (map-get? patient-privacy-settings { patient: patient }))
  )
    (ok {
      has-privacy-settings: (is-some privacy-settings),
      total-consents-granted: (var-get consent-counter), ;; Simplified for this implementation
      default-categories: (match privacy-settings settings (get default-categories settings) u31)
    })
  )
)

;; Helper function to check if consent is valid
(define-private (is-consent-valid (consent { doctor: principal, consultation-id: (optional uint), granted-categories: uint, access-level: uint, start-time: uint, expiry-time: uint, is-active: bool, emergency-override: bool, notes: (string-ascii 100) }))
  (and 
    (get is-active consent)
    (< stacks-block-height (get expiry-time consent))
    (> (get granted-categories consent) u0)
  )
)

;; Check if specific data category is accessible
(define-private (category-accessible (granted-categories uint) (requested-category uint))
  (> (bit-and granted-categories requested-category) u0)
)
