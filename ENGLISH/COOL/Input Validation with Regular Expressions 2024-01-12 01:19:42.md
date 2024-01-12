```cool
class Regex {
    String pattern;

    Regex(String pattern) {
        this.pattern = pattern;
    }

    boolean matches(String input) {
        return input.matches(pattern);
    }
}

class EmailValidator {
    Regex emailRegex = new Regex("[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}");

    boolean isValidEmail(String email) {
        return emailRegex.matches(email);
    }
}

class PhoneNumberValidator {
    Regex phoneNumberRegex = new Regex("^(\\+\\d{1,2}\\s)?\\(?\\d{3}\\)?[\\s.-]?\\d{3}[\\s.-]?\\d{4}$");

    boolean isValidPhoneNumber(String phoneNumber) {
        return phoneNumberRegex.matches(phoneNumber);
    }
}

class DateValidator {
    Regex dateRegex = new Regex("^(0?[1-9]|[1-2][0-9]|3[0-1])[/](0?[1-9]|1[0-2])[/](19|20)\\d\\d$");

    boolean isValidDate(String date) {
        return dateRegex.matches(date);
    }
}

class InputValidator {
    EmailValidator emailValidator = new EmailValidator();
    PhoneNumberValidator phoneNumberValidator = new PhoneNumberValidator();
    DateValidator dateValidator = new DateValidator();

    boolean isValidInput(String input) {
        return emailValidator.isValidEmail(input) || phoneNumberValidator.isValidPhoneNumber(input) || dateValidator.isValidDate(input);
    }
}

InputValidator inputValidator = new InputValidator();

String input = "example@domain.com";

System.out.println(inputValidator.isValidInput(input)); // true
```

Explanation:

* The `Regex` class represents a regular expression.
* The `EmailValidator`, `PhoneNumberValidator`, and `DateValidator` classes use regular expressions to validate email addresses, phone numbers, and dates, respectively.
* The `InputValidator` class uses the `EmailValidator`, `PhoneNumberValidator`, and `DateValidator` classes to validate input strings.
* The `isValidInput` method of the `InputValidator` class returns `true` if the input string is a valid email address, phone number, or date, and `false` otherwise.
* The `main` method creates an `InputValidator` object and calls the `isValidInput` method with the input string `"example@domain.com"`.
* The `isValidInput` method returns `true`, which is printed to the console.