# Telehealth Access Wallet

A decentralized telehealth platform enabling patients to consult verified doctors using STX micropayments.

## Features

- Doctor registration and verification
- Patient registration
- Consultation requests with STX payments
- Consultation management workflow
- Automated payment settlement

## Contract Functions

### For Doctors
- `register-doctor`: Register as a new doctor
- `accept-consultation`: Accept pending consultation requests
- `complete-consultation`: Mark consultations as completed

### For Patients
- `register-patient`: Register as a patient
- `request-consultation`: Request and pay for a consultation

### For Admin
- `verify-doctor`: Verify registered doctors

### Read-Only Functions
- `get-doctor`: Get doctor information
- `get-patient`: Get patient information
- `get-consultation`: Get consultation details

## Usage

1. Deploy the contract
2. Doctors register and get verified
3. Patients register and request consultations
4. Doctors accept and complete consultations
5. Payment automatically settles upon completion

## Consultation Fee
Current fee: 10 STX (configurable)
```