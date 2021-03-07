#include "stud_db.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

/** Student database */
struct StudentDatabase {
    struct Student* data;
    int initialized;
    int capacity;
    int size;
};

/** Database */
static struct StudentDatabase* db = 0;

void db_init(uint16_t initial_size)
{
    if (db == 0) {
        db = malloc(sizeof(struct StudentDatabase));
        db->data = calloc(initial_size, sizeof (struct Student));
        db->initialized = 1;
        db->capacity = initial_size;
        db->size = 0;
    } else {
        fprintf(stderr, "Database is already initialized!");
    }
}

void db_grow_if_needed() {
    /** Reallocate if full */
    if (db->size == db->capacity) {
        db->data = reallocarray(db->data, db->capacity * 2, sizeof(struct Student));
        db->capacity *= 2;
    }
}

void db_add(struct Student* student)
{
    db_grow_if_needed();

    /** Copy student data to next student */
    struct Student* next = &db->data[db->size];
    strncpy(next->name, student->name, MAX_NAME_LENGTH);
    strncpy(next->surname, student->surname, MAX_NAME_LENGTH);
    next->age = student->age;

    db->size += 1;
}

void db_list()
{
    printf("Students:\n");
    for (int i = 0; i < db->size; ++i) {
        struct Student* student = &db->data[i];
        printf("%s %s | Aged %d\n", student->name, student->surname, student->age);
    }
}

void db_destroy()
{
    free(db->data);
    free(db);
    db = 0;
}
