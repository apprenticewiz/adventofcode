#define _GNU_SOURCE
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define PCRE2_CODE_UNIT_WIDTH 8
#include <pcre2.h>

#define ASSIGN_OP  1
#define NOT_OP     2
#define AND_OP     3
#define OR_OP      4
#define LSHIFT_OP  5
#define RSHIFT_OP  6

typedef struct operation {
    int operator;
    char source1[4];
    union {
        char str[4];
        int amt;
    } source2;
} operation_t;

typedef struct op_node {
    char key[4];
    operation_t *value;
    struct op_node *left;
    struct op_node *right;
} op_node_t;

typedef struct op_table {
    op_node_t *root;
} op_table_t;

typedef struct cache_node {
    char key[4];
    int value;
    struct cache_node *left;
    struct cache_node *right;
} cache_node_t;

typedef struct cache_table {
    cache_node_t *root;
} cache_table_t;

void usage(char *progname) {
    fprintf(stderr, "usage: %s <input file>\n", progname);
    exit(1);
}

void put_op(op_node_t **root, char *key, operation_t *value) {
    op_node_t *nodeptr;
    int cmp;

    if ( *root == NULL ) {
        nodeptr = (op_node_t *)malloc(sizeof(op_node_t));
        memcpy(nodeptr->key, key, strlen(key));
	nodeptr->key[strlen(key)] = '\0';
        nodeptr->value = value;
        nodeptr->left = NULL;
        nodeptr->right = NULL;
        *root = nodeptr;
    } else {
        cmp = strcmp(key, (*root)->key);
        if ( cmp < 0 ) {
            put_op(&((*root)->left), key, value);
        } else if ( cmp > 0 ) {
            put_op(&((*root)->right), key, value);
        } else {
            memcpy((*root)->value, value, sizeof(operation_t));
        }
    }
}

operation_t *find_op(op_node_t *root, char *key) {
    int cmp;
 
    if ( root != NULL ) {
        cmp = strcmp(key, root->key);
        if ( cmp < 0 ) {
            return find_op(root->left, key);
        } else if ( cmp > 0 ) {
            return find_op(root->right, key);
        } else {
            return root->value;
        }
    } else {
        return NULL;
    }
}

void put_cache(cache_node_t **root, char *key, int value) {
    cache_node_t *nodeptr;
    int cmp;

    if ( *root == NULL ) {
        nodeptr = (cache_node_t *)malloc(sizeof(cache_node_t));
        memcpy(nodeptr->key, key, strlen(key));
	nodeptr->key[strlen(key)] = '\0';
        nodeptr->value = value;
        nodeptr->left = NULL;
        nodeptr->right = NULL;
        *root = nodeptr;
    } else {
        cmp = strcmp(key, (*root)->key);
        if ( cmp < 0 ) {
            put_cache(&((*root)->left), key, value);
        } else if ( cmp > 0 ) {
            put_cache(&((*root)->right), key, value);
        } else {
            (*root)->value = value;
        }
    }
}

int *find_cache(cache_node_t *root, char *key) {
    int cmp;
 
    if ( root != NULL ) {
        cmp = strcmp(key, root->key);
        if ( cmp < 0 ) {
            return find_cache(root->left, key);
        } else if ( cmp > 0 ) {
            return find_cache(root->right, key);
        } else {
            return &(root->value);
        }
    } else {
        return NULL;
    }
}

int all_digits(char *s) {
    char *p;

    for ( p = s; *p != '\0'; p++ ) {
       if ( !isdigit(*p) ) {
          return 0;
       }
    }
    return 1;
}

int eval(op_table_t *ops, cache_table_t *cache, char *expr) {
    operation_t *op;
    int *n;
    int result;

    if ( all_digits(expr) ) {
        return atoi(expr);
    } else if ( (n = find_cache(cache->root, expr)) != NULL ) {
        return *n;
    } else {
        op = find_op(ops->root, expr);
	switch ( op->operator ) {
	case ASSIGN_OP:
	    result = eval(ops, cache, op->source1);
            put_cache(&(cache->root), expr, result);
	    return result;
	case NOT_OP:
	    result = ~eval(ops, cache, op->source1) & 0xffff;
            put_cache(&(cache->root), expr, result);
	    return result;
	case AND_OP:
	    result = (eval(ops, cache, op->source1) & eval(ops, cache, op->source2.str)) & 0xffff;
            put_cache(&(cache->root), expr, result);
	    return result;
	case OR_OP:
	    result = (eval(ops, cache, op->source1) | eval(ops, cache, op->source2.str)) & 0xffff;
            put_cache(&(cache->root), expr, result);
	    return result;
	case LSHIFT_OP:
	    result = (eval(ops, cache, op->source1) << op->source2.amt) & 0xffff;
            put_cache(&(cache->root), expr, result);
	    return result;
	case RSHIFT_OP:
	    result = (eval(ops, cache, op->source1) >> op->source2.amt) & 0xffff;
            put_cache(&(cache->root), expr, result);
	    return result;
	}
    }
}

int process(char *filename) {
    FILE *infile;
    char *line;
    size_t size;
    int i;
    int error_number;
    PCRE2_SIZE error_offset;
    PCRE2_UCHAR buffer[256];
    PCRE2_SPTR patterns[] = {
        (PCRE2_SPTR)"^(\\d+|\\w+) -> (\\w+)$",
        (PCRE2_SPTR)"NOT (\\d+|\\w+) -> (\\w+)",
        (PCRE2_SPTR)"(\\d+|\\w+) (AND|OR) (\\d+|\\w+) -> (\\w+)",
        (PCRE2_SPTR)"(\\d+|\\w+) (LSHIFT|RSHIFT) (\\d+) -> (\\w+)",
    };
    pcre2_code *regexes[4];
    pcre2_match_data *match_data[4];
    PCRE2_SIZE *ovector;
    int rc;
    PCRE2_SIZE start, end, len;
    op_table_t op_table;
    operation_t *op;
    char dest[4];
    char tmp_str[16];
    cache_table_t cache;
    int a;

    op_table.root = NULL;
    cache.root = NULL;
    if ( (infile = fopen(filename, "r")) == NULL ) {
        perror("failed to open input file");
        exit(1);
    }
    line = NULL;
    for ( i = 0; i < 4; i++ ) {
        regexes[i] = pcre2_compile(patterns[i], PCRE2_ZERO_TERMINATED, 0, &error_number, &error_offset, NULL);
        if ( regexes[i] == NULL ) {
            pcre2_get_error_message(error_number, buffer, sizeof(buffer));
            fprintf(stderr, "pcre2 error: compilation failed at offset %d: %s\n", (int)error_number, buffer);
        }
        match_data[i] = pcre2_match_data_create_from_pattern(regexes[i], NULL);
    }
    while ( getline(&line, &size, infile) != -1 ) {
        for ( i = 0; i < 4; i++ ) {
            rc = pcre2_match(regexes[i], (PCRE2_SPTR)line, strlen(line), 0, 0, match_data[i], NULL);
            if ( rc > 0 ) {
                ovector = pcre2_get_ovector_pointer(match_data[i]);
                op = (operation_t *)malloc(sizeof(operation_t));
                break;
            }
        }
        switch ( i ) {
        case 0:  /* assign regex */
            op->operator = ASSIGN_OP;
            start = ovector[2];
            end = ovector[3];
            len = end - start;
            memcpy(op->source1, line + start, len);
            op->source1[len] = '\0';
            start = ovector[4];
            end = ovector[5];
            len = end - start;
            memcpy(dest, line + start, len);
            dest[len] = '\0';
            put_op(&op_table.root, dest, op);
            break;
        case 1:  /* not regex */
            op->operator = NOT_OP;
            start = ovector[2];
            end = ovector[3];
            len = end - start;
            memcpy(op->source1, line + start, len);
            op->source1[len] = '\0';
            start = ovector[4];
            end = ovector[5];
            len = end - start;
            memcpy(dest, line + start, len);
            dest[len] = '\0';
            put_op(&op_table.root, dest, op);
            break;
        case 2:  /* and/or regex */
            start = ovector[2];
            end = ovector[3];
            len = end - start;
            memcpy(op->source1, line + start, len);
            op->source1[len] = '\0';
            start = ovector[4];
            end = ovector[5];
            len = end - start;
            memcpy(tmp_str, line + start, len);
            tmp_str[len] = '\0';
            if ( strcmp(tmp_str, "AND") == 0 ) {
                op->operator = AND_OP;
            } else {
                op->operator = OR_OP;
            }
            start = ovector[6];
            end = ovector[7];
            len = end - start;
            memcpy(op->source2.str, line + start, len);
            op->source2.str[len] = '\0';
            start = ovector[8];
            end = ovector[9];
            len = end - start;
            memcpy(dest, line + start, len);
            dest[len] = '\0';
            put_op(&op_table.root, dest, op);
            break;
        case 3:  /* shift regex */
            start = ovector[2];
            end = ovector[3];
            len = end - start;
            memcpy(op->source1, line + start, len);
            op->source1[len] = '\0';
            start = ovector[4];
            end = ovector[5];
            len = end - start;
            memcpy(tmp_str, line + start, len);
            tmp_str[len] = '\0';
            if ( strcmp(tmp_str, "LSHIFT") == 0 ) {
                op->operator = LSHIFT_OP;
            } else {
                op->operator = RSHIFT_OP;
            }
            start = ovector[6];
            end = ovector[7];
            len = end - start;
            memcpy(tmp_str, line + start, len);
            tmp_str[len] = '\0';
            op->source2.amt = atoi(tmp_str);
            start = ovector[8];
            end = ovector[9];
            len = end - start;
            memcpy(dest, line + start, len);
            dest[len] = '\0';
            put_op(&op_table.root, dest, op);
        }
        free(line);
        line = NULL;
    }
    fclose(infile);
    for ( i = 0; i < 4; i++ ) {
        pcre2_match_data_free(match_data[i]);
        pcre2_code_free(regexes[i]);
    }
    a = eval(&op_table, &cache, "a");
    op = (operation_t *)malloc(sizeof(operation_t));
    op->operator = ASSIGN_OP;
    sprintf(op->source1, "%d", a);
    memcpy(dest, "b", 1);
    dest[1] = '\0';
    put_op(&op_table.root, dest, op);
    cache.root = NULL;
    return eval(&op_table, &cache, "a");
}

int main(int argc, char *argv[]) {
    char *filename;
    int result;

    if ( argc < 2 ) {
        usage(argv[0]);
    }

    filename = argv[1];
    result = process(filename);
    printf("result = %d\n", result);
    return 0;
}
