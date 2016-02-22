/*
------------------------------------------------------------------------------
-- The ADP Compiler 
-- Copyright (C) 2001-2008 Peter Steffen, Christian Lang, Marco Ruether, 
--                         Georg Sauthoff, Stefanie Schirmer
--
-- Send comments/bug reports to: P.Steffen <psteffen@techfak.uni-bielefeld.de>.
-- Updates: http://bibiserv.techfak.uni-bielefeld.de/adp/adpcomp.html
------------------------------------------------------------------------------

This file is part of ADPC (The ADP Compiler).

ADPC is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or
(at your option) any later version.

ADPC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with ADPC.  If not, see <http://www.gnu.org/licenses/>.
*/

#ifndef _GRAPH_H_
#define _GRAPH_H_

#include "dll.h"

/*! \file graph.h
    \brief A graph ADT.

    Uses internal an array of vertex pointers. Childs and Parents of a
    vertex are arrays of edge pointers. edge(i, j) has the same memory
    representation as edge(j, i).

    Use some bounds checks, i.e. internal arrays gets resized if something
    is inserted outside current bounds (e.g. vertices, edges).
 */

struct graph;
struct edge;

typedef void (*vertex_function)(struct graph* g, int i, void* extra);
typedef void (*edge_function)(struct graph* g, int i, int j, void* extra);

#ifndef UNSAFE
#define SAFE
#endif

/*! \brief Edge Element. */
struct edge {
  int from; /*!< vertex index */
  int to; /*!< vertex index */
};

/*! \brief A arraylist of edges. */
struct edges {
  int count; /*!< number of inserted non-deleted edges */
  int last_index; /*!< last index ready for new element */
  int bound; /*!< last usable index before need to reallocate */
  struct edge **array;
};

/*! \brief Base vertex containing in/out edges as array lists. */
struct vertex {
  struct edges in;
  struct edges out;
};

/*! \brief Graph ADT. */
struct graph {
  struct vertex **array; /*!< array of vertices as pointers */

  int vertex_count; /*!< number of vertices */
  int edge_count; /*!< number of edges */
  
  int last_index; /*!< last index where you can put a new element */
  int bound; /*!< last index where you can write in the vertex array */
  int vertex_step_size; /*!< in which steps vertex array should be
                          extended */
  int edge_step_size; /*!< in which steps edge arrays should be extended
                       */

  vertex_function update; /*!< function which is called evertime, a property of
                            a vertex changes */

  size_t graph_size; /*!< size in bytes of the graph element */
  size_t vertex_size; /*!< size in bytes of one vertex element */
  size_t edge_size; /*!< size in bytes of one edge element */
};

/*! \brief Edge list element. */
struct edge_head {
  struct list_head head;
  struct edge *edge;
};

/*! \brief Edge iterator representation. */
struct edge_iterator {
  struct edges *edges; /*!< edge pointer array */
  int current_index; /*!< the current index at the edge pointer array */
};

/*! \brief Vertex iterator representation. */
struct vertex_iterator {
  struct graph *graph; /*!< graph */
  int current_index; /*!< current vertex index while iterating */
};

/*! \brief Initializes the graph ADT
    \param initial_vertices the initial size of the vertex pointer array
    \param vertex_step_size in which steps resize the vertex array
    \param edge_step_size in which steps resize the edge arrays
    \param graph_size size of graph struct in bytes
    \param vertex_size size of vertex struct in bytes
    \param edge_size size of edge struct in bytes
    \param update function, which should get called, if one vertex
                  change its properties (e.g. incoming edge deleted),
                  or NULL for no function 
    \return new allocated graph ADT
   */
struct graph* graph_init(
        int initial_vertices,
        int vertex_step_size,
        int edge_step_size,
        size_t graph_size,
        size_t vertex_size,
        size_t edge_size,
        vertex_function update);

/*! \brief Returns k. vertex from array. */
struct vertex *graph_vertex(struct graph *g, int k);

/*! \brief Returns the pointer to the specified edge. */
struct edge *graph_edge(struct graph *g, int k, int l);

/*! \brief Adds new vertex to graph
    \return new allocated vertex object
 */
struct vertex* graph_add_vertex(struct graph* g, int k);

/*! \brief Removes vertex from graph.
    \return removed vertex object
 */
struct vertex* graph_remove_vertex(struct graph* g, int k);

/*! \brief Removes a vertex from the graph and list it edges.
    \param e_head empty list, which gets filled with
                \f$edges = \in in(k) \cup out(k) \f$
    \return removed vertex object
 */
struct vertex* graph_remove_vertex_e(struct graph* g, int k,
    struct list_head **e_head);

/*! \brief Tests if graph contains the edge.
    \return !=0 if graph contains the edge, 0 else
 */
int edge_exists(struct graph* g, int i, int j);

/*! \brief Inserts an edge into the graph.
    \return the new allocated edge */
struct edge *graph_insert_edge(struct graph* g, int i, int j);

/*! \brief Removes an edge from the graph.
    \return the removed edge object */
struct edge *graph_remove_edge(struct graph* g, int i, int j);

/*! \brief Makes a deep copy from a graph.
    \return new allocated copied graph
 */
struct graph* graph_copy(struct graph* g);

/*! \brief Traverses all vertices and call foreach the callback function.
    \param fn callback function
    \param extra some extra information, which is passed to each callback
 */
void graph_walk_vertices(struct graph* g, vertex_function fn, void* extra);

/*! \brief Traverses all edges and call foreach the callback function.
    \param fn callback function
    \param extra some extra information which is passed to each callback
 */
void graph_walk_edges(struct graph* g, edge_function fn, void *extra);

/*! \brief Prints all edges of the graph. */
void graph_print_edges(struct graph *g);

/*! \brief Prints all vertices of the graph. */
void graph_print_vertices(struct graph *g);

/*! \brief Frees the all memory allocated by the ADT.

  Including g.
 */
void graph_delete(struct graph *g);

/*! \brief Reads a graph from file.

    Textfile; one edge at a line, numbers seperated by one space
 */
void graph_read(struct graph *g, char *filename);

/*! \brief Returns an iterator for an array of edges.
    \param i adress of iterator which should be initialised
             or NULL, if function should allocated some new one
             - but don't forget to free it then
 */
struct edge_iterator *graph_edge_iterator(struct edges *e,
    struct edge_iterator *i);

/*! \brief Returns next edge in the edge array.
    \return next edge address or NULL if no edge left */
struct edge *graph_edge_itr_next(struct edge_iterator *i);

/*! \brief Returns an iterator for the graph vertices.
    \param i adress of iterator which should be initialized
             if NULL, if function should allocate one
 */
struct vertex_iterator *graph_vertex_iterator(struct graph *g,
    struct vertex_iterator *i);

/*! \brief Return next vertex from the graph.
    \return address of next vertex or NULL, if no vertex left
 */
struct vertex *graph_vertex_itr_next(struct vertex_iterator *i);

#endif
