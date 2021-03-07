#include "stud_db.h"

#include <ctype.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/** Some compile time constants */
#define MIN_AGE 18
#define MAX_AGE 130
#define NAME_MIN_LEN 2
#define SURNAME_MIN_LEN 4

/** Based on the max names and spaces between, to avoid dynamic memory */
#define CMD_MAX_LENGTH (MAX_NAME_LENGTH * 2 + 3 + 4 + 4)

/** Command parse results */
enum PARSE_RESULT
{
    PARSE_RUN,
    PARSE_QUIT,
};

/** All possible errors as a bit flag so we can combine them easily */
enum ERROR_FLAGS
{
    VALID                   = 0U,
    NAME_SHORT              = 1U << 1U,
    NAME_NON_CAPITALIZED    = 1U << 2U,
    NAME_INVALID_LETTER     = 1U << 3U,
    SURNAME_SHORT           = 1U << 4U,
    SURNAME_NON_CAPITALIZED = 1U << 5U,
    SURNAME_INVALID_LETTER  = 1U << 6U,
    AGE_LOW                 = 1U << 7U,
    AGE_HIGH                = 1U << 8U,
    AGE_NOT_A_NUMBER        = 1U << 9U,
};

/** Error strings */
const char* const ERROR_STRINGS[] = {
    "Name is too short. ",
    "Name is not capitalized. ",
    "Name error: only [a-Z] allowed. ",
    "Surname is too short. ",
    "Surname is not capitalized. ",
    "Surname error: only [a-Z] allowed. ",
    "Age is below 18. ",
    "Age is above 130. ",
    "Age is not a number. ",
};

/**
 * Validate a student by checking their data
 *
 * @return A bitflag of all error states, or VALID if no errors
 */
unsigned validate_student(struct Student* student)
{
    unsigned err = 0U;

    /** Age */
    if (student->age == -1)
    {
        err |= AGE_NOT_A_NUMBER;
    }
    else if (student->age < MIN_AGE)
    {
        err |= AGE_LOW;
    }
    else if (student->age > MAX_AGE)
    {
        err |= AGE_HIGH;
    }

    /** First name */
    if (strnlen(student->name, MAX_NAME_LENGTH) < NAME_MIN_LEN)
    {
        err |= NAME_SHORT;
    }

    for (size_t i = 0; i < strnlen(student->name, MAX_NAME_LENGTH); ++i)
    {
        if (i == 0 && islower(student->name[0]))
        {
            err |= NAME_NON_CAPITALIZED;
        }

        if (!isalpha(student->name[i]))
        {
            err |= NAME_INVALID_LETTER;
        }
    }

    /** Last name - duplicated on purpose */
    if (strnlen(student->surname, MAX_NAME_LENGTH) < SURNAME_MIN_LEN)
    {
        err |= SURNAME_SHORT;
    }

    for (size_t i = 0; i < strnlen(student->surname, MAX_NAME_LENGTH); ++i)
    {
        if (i == 0 && islower(student->surname[0]))
        {
            err |= SURNAME_NON_CAPITALIZED;
        }

        if (!isalpha(student->surname[i]))
        {
            err |= SURNAME_INVALID_LETTER;
        }
    }

    return err;
}

/** Parse error enum and print errors */
void print_errors(unsigned err)
{
    // clang-format off

    printf("\nERRORS: [");
    if (err & NAME_SHORT) { printf("%s", ERROR_STRINGS[0]); }
    if (err & NAME_NON_CAPITALIZED) { printf("%s", ERROR_STRINGS[1]); }
    if (err & NAME_INVALID_LETTER) { printf("%s", ERROR_STRINGS[2]); }
    if (err & SURNAME_SHORT) { printf("%s", ERROR_STRINGS[3]); }
    if (err & SURNAME_NON_CAPITALIZED) { printf("%s", ERROR_STRINGS[4]); }
    if (err & SURNAME_INVALID_LETTER) { printf("%s", ERROR_STRINGS[5]); }
    if (err & AGE_LOW) { printf("%s", ERROR_STRINGS[6]); }
    if (err & AGE_HIGH) { printf("%s", ERROR_STRINGS[7]); }
    if (err & AGE_NOT_A_NUMBER) { printf("%s", ERROR_STRINGS[8]); }
    printf("]\n");

    // clang-format on
}

int parse_command(char* cmd)
{
    if (strncmp("list", cmd, 4) == 0)
    {
        db_list();
    }
    else if (strncmp("end", cmd, 3) == 0)
    {
        return PARSE_QUIT;
    }
    else
    {
        struct Student student;
        char age_s[MAX_NAME_LENGTH];
        if (sscanf(cmd, "new %s %s %s", student.name, student.surname, age_s) != 3)
        {
            fprintf(stderr, "\nERROR: Format: new First Surname Age[number])!\n");
            return PARSE_RUN;
        }
        char* ptr;
        student.age = (int)strtol(age_s, &ptr, 10);
        if (errno != 0)
        {
            student.age = -1; // let's use it as NOT_A_NUMBER error
        }
        unsigned err = validate_student(&student);

        if (err == VALID)
        {
            db_add(&student);
        }
        else
        {
            print_errors(err);
        }
    }

    return PARSE_RUN;
}

int read_and_execute_command()
{
    char cmd_buf[CMD_MAX_LENGTH];
    fgets(cmd_buf, CMD_MAX_LENGTH, stdin);

    /** Ensure we got some input */
    if (strnlen(cmd_buf, CMD_MAX_LENGTH) == 0)
    {
        fprintf(stderr, "Command can not be empty!");
        return PARSE_RUN;
    }

    /** Parse command */
    return parse_command(cmd_buf);
}

int main()
{
    db_init(1000);

    while (1)
    {
        if (read_and_execute_command() == PARSE_QUIT)
        {
            break;
        }
    }

    db_destroy();
    return 0;
}
