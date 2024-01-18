```prolog
/* Expert System for Medical Diagnosis */

:- dynamic fact/1.
:- dynamic rule/2.

% Facts about symptoms and diseases
fact(headache).
fact(fever).
fact(cough).
fact(sore_throat).
fact(runny_nose).
fact(nausea).
fact(vomiting).
fact(diarrhea).
fact(rash).
fact(itching).
fact(chest_pain).
fact(shortness_of_breath).
fact(abdominal_pain).
fact(back_pain).
fact(joint_pain).
fact(muscle_pain).
fact(fatigue).
fact(weakness).
fact(weight_loss).
fact(loss_of_appetite).

% Rules for diagnosing diseases
rule(flu, [headache, fever, cough, sore_throat, runny_nose]).
rule(common_cold, [headache, fever, cough, sore_throat, runny_nose]).
rule(pneumonia, [cough, shortness_of_breath, chest_pain]).
rule(bronchitis, [cough, shortness_of_breath, wheezing]).
rule(sinusitis, [headache, fever, facial_pain, nasal_congestion]).
rule(strep_throat, [sore_throat, fever, headache, body_aches]).
rule(tonsillitis, [sore_throat, fever, swollen_tonsils]).
rule(ear_infection, [ear_pain, fever, hearing_loss]).
rule(conjunctivitis, [redness, swelling, itching, tearing]).
rule(gastroenteritis, [nausea, vomiting, diarrhea]).
rule(food_poisoning, [nausea, vomiting, diarrhea, abdominal_pain]).
rule(appendicitis, [abdominal_pain, nausea, vomiting, fever]).
rule(diverticulitis, [abdominal_pain, fever, nausea, vomiting]).
rule(ulcerative_colitis, [abdominal_pain, diarrhea, bloody_stool]).
rule(crohn's_disease, [abdominal_pain, diarrhea, weight_loss, fever]).
rule(irritable_bowel_syndrome, [abdominal_pain, diarrhea, constipation]).
rule(skin_rash, [rash, itching]).
rule(eczema, [rash, itching, redness]).
rule(psoriasis, [rash, itching, scaling]).
rule(hives, [rash, itching, swelling]).
rule(chickenpox, [rash, itching, fever]).
rule(measles, [rash, fever, cough, runny_nose]).
rule(mumps, [swelling_of_the_parotid_glands, fever, headache]).
rule(rubella, [rash, fever, swollen_lymph_nodes]).
rule(scarlet_fever, [rash, fever, sore_throat, swollen_lymph_nodes]).
rule(rheumatoid_arthritis, [joint_pain, stiffness, swelling]).
rule(osteoarthritis, [joint_pain, stiffness, difficulty_moving]).
rule(gout, [joint_pain, swelling, redness]).
rule(fibromyalgia, [muscle_pain, fatigue, stiffness]).
rule(chronic_fatigue_syndrome, [fatigue, weakness, sleep_problems, muscle_pain]).
rule(cancer, [weight_loss, fatigue, weakness, loss_of_appetite]).

% Query the user about their symptoms
write('Enter your symptoms (separated by commas): '), nl,
    read_line_to_codes(user_input, Codes),
    atom_codes(Symptoms, Codes),
    atom_codes(' ', Separator),
    phrase(symptom_list, Symptoms, Separator).

% Find all diseases that match the user's symptoms
possible_diseases(Diseases) :-
    findall(Disease, (rule(Disease, Symptoms), member(Symptom, Symptoms)), Diseases).

% Print the possible diseases
write('Possible diseases: '), nl,
    foreach(Disease in possible_diseases(Diseases)) do
        write(Disease), nl.
```

Explanation:

1. **Facts:** The first section of the code defines a list of facts about symptoms and diseases. These facts are represented using the `fact/1` predicate, where the argument is the symptom or disease.

2. **Rules:** The second section of the code defines a set of rules for diagnosing diseases. These rules are represented using the `rule/2` predicate, where the first argument is the disease and the second argument is a list of symptoms associated with that disease.

3. **Querying the User:** The third section of the code prompts the user to enter their symptoms, reads the user's input, and converts it into a list of symptoms.

4. **Finding Possible Diseases:** The fourth section of the code uses the `possible_diseases/1` predicate to find all diseases that match the user's symptoms. This predicate first finds all rules where the symptoms in the rule match the user's symptoms. It then extracts the disease associated with each rule and returns a list of possible diseases.

5. **Printing the Possible Diseases:** The fifth and final section of the code simply prints the list of possible diseases to the user.