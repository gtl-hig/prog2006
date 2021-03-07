#include <stdint.h>

/** To avoid having to deal with too much dynamic memory :P */
#define MAX_NAME_LENGTH 128

/** Simple collection of student data */
struct Student
{
    char name[MAX_NAME_LENGTH];
    char surname[MAX_NAME_LENGTH];
    int age;
};

/**
 * Initialize student database
 *
 * @param initial_size Initial size of database
 * @note only call once at launch
 */
void db_init(uint16_t initial_size);

/**
 * Add a student to the in-memory database
 * @param student The student to add, it should not be on the heap, the db will make a copy
 */
void db_add(struct Student* student);

/**
 * List all entries in database
 */
void db_list();

/**
 * Call before shutting down program to free db memory
 */
void db_destroy();
