#include <stdlib.h>

// Structures to play with

typedef struct point {
    int x;
    int y;
} point_t;

typedef struct edge {
    struct point start;
    struct point end;
} edge_t;

typedef struct graph {
    struct point *points;
    struct edge *edges;
} graph_t;


// A few test functions that claim and release data

void * ker_malloc(int size, int id) {
    return malloc(size);
}

void ker_free(void *buffer) {
    free(buffer);
}

graph_t* __attribute__((sos_claim)) make_graph_return_bad_a(int size) {
    point_t *points;
    edge_t *edges;
    graph_t *g;

    points = ker_malloc(sizeof(struct point) * size, 1);
    edges = ker_malloc(sizeof(struct edge) * size, 1);
    
    // Note that memory for graph g is not allocated
    g->points = points;
    g->edges = edges;

    return g;
}


graph_t* __attribute__((sos_claim)) make_graph_return_bad_b(int size) {
    point_t *points;
    edge_t *edges;
    graph_t *g;

    points = ker_malloc(sizeof(struct point) * size, 1);
    edges = ker_malloc(sizeof(struct edge) * size, 1);
    
    // Note that memory for graph g is not allocated on all paths
    if (size % 2 == 0) {
        g = ker_malloc(sizeof(struct graph), 1);
    }
    g->points = points;
    g->edges = edges;

    return g;
}


graph_t* __attribute__((sos_claim)) make_graph_return(int size) {
    point_t *points;
    edge_t *edges;
    graph_t *g;

    points = ker_malloc(sizeof(struct point) * size, 1);
    edges = ker_malloc(sizeof(struct edge) * size, 1);
    g = ker_malloc(sizeof(struct graph), 1);
    g->points = points;
    g->edges = edges;

    return g;
}


void make_graph_formal_bad_a(int size, graph_t *new_graph __attribute__((sos_claim))) {
    point_t *points;
    edge_t *edges;
    graph_t *g;

    points = ker_malloc(sizeof(struct point) * size, 1);
    edges = ker_malloc(sizeof(struct edge) * size, 1);
   
    // Note that memory for new_graph (via g) is not allocated!!! 
    g->points = points;
    g->edges = edges;
    new_graph = g;

    return;
}

void make_graph_formal_bad_b(int size, graph_t *new_graph __attribute__((sos_claim))) {
    point_t *points;
    edge_t *edges;
    graph_t *g;

    points = ker_malloc(sizeof(struct point) * size, 1);
    edges = ker_malloc(sizeof(struct edge) * size, 1);
   
    // Note that memory for new_graph (via g) is not allocated on all paths...
    
    if((size % 2) == 0) {
        g = ker_malloc(sizeof(struct graph), 1);
    }

    g->points = points;
    g->edges = edges;
    new_graph = g;

    return;
}

void make_graph_formal(int size, graph_t *new_graph __attribute__((sos_claim))) {
    point_t *points;
    edge_t *edges;
    graph_t *g;

    points = ker_malloc(sizeof(struct point) * size, 1);
    edges = ker_malloc(sizeof(struct edge) * size, 1);
    
    g = ker_malloc(sizeof(struct graph), 1);
    g->points = points;
    g->edges = edges;
    new_graph = g;
    
    return;
}

void free_graph(struct graph * g __attribute__((sos_release))) {
    free((void *) (g->points));
    free((void *) (g->edges));
    free((void *) g);
}


// Set up data for testing!!!

int main() {

    graph_t *g;
    graph_t *ga;
    graph_t *gna;
    
    point_t *p;
    point_t *pa;
    
    
    g = make_graph_return(10);
    ga = g;
    ga->points[0].x = 3;
    ga->points[0].y = 1;
    ga->edges[2].start.x = 4;
    ga->edges[2].start.y = 1;
 
    make_graph_formal(10, gna);;   

    // ga and g alias
    // gna aliasis nothing

ONE:
    
  
TWO:

    free_graph(g);
   
THREE:

    return 0;
}


