```typescript
// Import required modules
import { Component, OnInit, OnDestroy } from '@angular/core';
import { Subscription } from 'rxjs';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import { ActivatedRoute, Router } from '@angular/router';

// Component Decorator
@Component({
  selector: 'app-complex-form',
  templateUrl: './complex-form.component.html',
  styleUrls: ['./complex-form.component.scss'],
})
export class ComplexFormComponent implements OnInit, OnDestroy {
  // Component Properties
  complexForm: FormGroup;
  submitted = false;
  loading = false;
  subscription: Subscription;
  id: string;
  editMode: boolean;

  // Constructor
  constructor(
    private formBuilder: FormBuilder,
    private activatedRoute: ActivatedRoute,
    private router: Router
  ) {}

  // Lifecycle Hook - Angular OnInit
  ngOnInit(): void {
    // Initialize Form
    this.complexForm = this.formBuilder.group({
      firstName: ['', [Validators.required, Validators.minLength(3)]],
      lastName: ['', [Validators.required, Validators.minLength(3)]],
      email: ['', [Validators.required, Validators.email]],
      phoneNumber: ['', [Validators.required, Validators.pattern('^[0-9]{10}$')]],
      address: ['', Validators.required],
      city: ['', Validators.required],
      state: ['', Validators.required],
      zipCode: ['', [Validators.required, Validators.pattern('^[0-9]{5}$')]],
      country: ['', Validators.required],
      gender: ['', Validators.required],
      dob: ['', Validators.required],
      maritalStatus: ['', Validators.required],
      education: ['', Validators.required],
      employmentStatus: ['', Validators.required],
      occupation: ['', Validators.required],
      annualIncome: ['', Validators.required],
      creditScore: ['', Validators.required],
      loanAmount: ['', Validators.required],
      loanPurpose: ['', Validators.required],
      loanTerm: ['', Validators.required],
      loanInterestRate: ['', Validators.required],
      monthlyPayment: ['', Validators.required],
      totalInterestPaid: ['', Validators.required],
      totalLoanCost: ['', Validators.required],
    });

    // Subscribe to Route Parameters
    this.subscription = this.activatedRoute.params.subscribe((params) => {
      if (params['id']) {
        this.id = params['id'];
        this.editMode = true;
        // Load Data from API
        this.loadData();
      }
    });
  }

  // Lifecycle Hook - Angular OnDestroy
  ngOnDestroy(): void {
    this.subscription.unsubscribe();
  }

  // Load Data
  loadData(): void {
    // API Call to Load Data
    this.loading = true;
    setTimeout(() => {
      this.complexForm.patchValue({
        firstName: 'John',
        lastName: 'Doe',
        email: 'johndoe@example.com',
        phoneNumber: '1234567890',
        address: '123 Main Street',
        city: 'Anytown',
        state: 'CA',
        zipCode: '12345',
        country: 'USA',
        gender: 'Male',
        dob: '1980-01-01',
        maritalStatus: 'Married',
        education: 'Bachelor's Degree',
        employmentStatus: 'Employed',
        occupation: 'Software Engineer',
        annualIncome: '100000',
        creditScore: '700',
        loanAmount: '10000',
        loanPurpose: 'Personal',
        loanTerm: '36',
        loanInterestRate: '5',
        monthlyPayment: '300',
        totalInterestPaid: '2000',
        totalLoanCost: '12000',
      });
      this.loading = false;
    }, 1000);
  }

  // Form Submission
  onSubmit(): void {
    this.submitted = true;

    if (this.complexForm.invalid) {
      return;
    }

    if (this.editMode) {
      // API Call to Update Data
    } else {
      // API Call to Create New Data
    }
  }

  // Reset Form
  onReset(): void {
    this.submitted = false;
    this.complexForm.reset();
  }

  // Go Back
  onBack(): void {
    this.router.navigate(['./']);
  }
}
```

**Explanation:**

This code creates a complex form in Angular, where the user can enter personal, financial, and loan-related information. The form has various fields, including first name, last name, email, phone number, address, city, state, zip code, country, gender, date of birth, marital status, education, employment status, occupation, annual income, credit score, loan amount, loan purpose, loan term, loan interest rate, monthly payment, total interest paid, and total loan cost.

The component uses the FormBuilder to create a FormGroup, which is a collection of form controls. Each form control represents a single input field. The form control has various validators, which define the rules for validating the input.

The component also uses the ActivatedRoute and Router services to handle routing. When the component is initially loaded, it checks if there is an 'id' parameter in the route. If there is, it means the component is in edit mode, and it loads the data from the API.

The component has methods for submitting the form, resetting the form, and going back to the previous page.

This code demonstrates the use of various Angular features, including forms, routing, and data loading. It is a good example of how to create a complex form in Angular.