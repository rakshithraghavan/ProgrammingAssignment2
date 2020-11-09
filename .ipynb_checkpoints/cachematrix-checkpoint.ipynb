{
 "cells": [
  {
   "cell_type": "code",
   "execution_count": 2,
   "metadata": {},
   "outputs": [],
   "source": [
    "## Input x as a matrix\n",
    "## and then set \"s\" value as null\n",
    "## then I changed reference from \"mean\" to \"solve\""
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 3,
   "metadata": {},
   "outputs": [],
   "source": [
    "makeCacheMatrix <- function(x = matrix(sample(1:100,9),3,3)) {\n",
    "  s <- NULL\n",
    "  set <- function(y) {\n",
    "    x <<- y\n",
    "    s <<- NULL\n",
    "  }\n",
    "  get <- function() x\n",
    "  setsolve <- function(solve) s <<- solve\n",
    "  getsolve <- function() s\n",
    "  list(set = set, get = get,\n",
    "       setsolve = setsolve,\n",
    "       getsolve = getsolve)\n",
    "}"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 4,
   "metadata": {},
   "outputs": [],
   "source": [
    "## Similarly, changed \"mean\" to \"solve\".\n",
    "cacheSolve <- function(x, ...) {\n",
    "  s <- x$getsolve()\n",
    "  if(!is.null(s)) {\n",
    "    message(\"Inversed Matrix\")\n",
    "    return(s)\n",
    "  }\n",
    "  data <- x$get()\n",
    "  s <- solve(data, ...)\n",
    "  x$setsolve(s)\n",
    "  s\n",
    "}"
   ]
  },
  {
   "cell_type": "markdown",
   "metadata": {},
   "source": [
    "## Our aim is to write a pair of functions, namely, \"makeCacheMatrix\" and \"cacheSolve\" that cache the inverse of a matrix\n",
    "\n",
    "## makeCacheMatrix is a function which creates a special \"matrix\" object that can cache its inverse for the input."
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 5,
   "metadata": {},
   "outputs": [],
   "source": [
    "makeCacheMatrix <- function(x = matrix()) {\n",
    "\n",
    "inv <- NULL\n",
    "set <- function(y) {\n",
    "x <<- y\n",
    "inv <<- NULL\n",
    "}\n",
    "get <- function() x\n",
    "setinv <- function(inverse) inv <<- inverse\n",
    "getinv <- function() inv\n",
    "list(set = set, get = get, setinv = setinv, getinv = getinv)\n",
    "}"
   ]
  },
  {
   "cell_type": "markdown",
   "metadata": {},
   "source": [
    "## CacheSolve is a function which computes the inverse of the special \"matrix\" returned by makeCacheMatrix above. If the inverse has already been calculated, then the cachesolve should retrieve the inverse from the cache."
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 6,
   "metadata": {},
   "outputs": [],
   "source": [
    "cacheSolve <- function(x, ...) {\n",
    "## Return a matrix that is the inverse of 'x'\n",
    "inv <- x$getinv()\n",
    "if(!is.null(inv)) {\n",
    "message(\"getting cached result\")\n",
    "return(inv)\n",
    "}\n",
    "data <- x$get()\n",
    "inv <- solve(data, ...)\n",
    "x$setinv(inv)\n",
    "inv\n",
    "}"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 7,
   "metadata": {},
   "outputs": [
    {
     "data": {
      "text/html": [
       "<table>\n",
       "<caption>A matrix: 4 × 4 of type dbl</caption>\n",
       "<tbody>\n",
       "\t<tr><td> 0.02051121</td><td>-0.6607326</td><td> 0.05388656</td><td> 0.2158500</td></tr>\n",
       "\t<tr><td> 1.08677588</td><td>-0.3911602</td><td> 0.40524185</td><td> 0.2449156</td></tr>\n",
       "\t<tr><td>-0.05750143</td><td>-0.1445250</td><td> 0.27654390</td><td> 0.3725861</td></tr>\n",
       "\t<tr><td> 0.34273597</td><td> 1.5545848</td><td>-0.81929860</td><td>-0.1392084</td></tr>\n",
       "</tbody>\n",
       "</table>\n"
      ],
      "text/latex": [
       "A matrix: 4 × 4 of type dbl\n",
       "\\begin{tabular}{llll}\n",
       "\t  0.02051121 & -0.6607326 &  0.05388656 &  0.2158500\\\\\n",
       "\t  1.08677588 & -0.3911602 &  0.40524185 &  0.2449156\\\\\n",
       "\t -0.05750143 & -0.1445250 &  0.27654390 &  0.3725861\\\\\n",
       "\t  0.34273597 &  1.5545848 & -0.81929860 & -0.1392084\\\\\n",
       "\\end{tabular}\n"
      ],
      "text/markdown": [
       "\n",
       "A matrix: 4 × 4 of type dbl\n",
       "\n",
       "|  0.02051121 | -0.6607326 |  0.05388656 |  0.2158500 |\n",
       "|  1.08677588 | -0.3911602 |  0.40524185 |  0.2449156 |\n",
       "| -0.05750143 | -0.1445250 |  0.27654390 |  0.3725861 |\n",
       "|  0.34273597 |  1.5545848 | -0.81929860 | -0.1392084 |\n",
       "\n"
      ],
      "text/plain": [
       "     [,1]        [,2]       [,3]        [,4]      \n",
       "[1,]  0.02051121 -0.6607326  0.05388656  0.2158500\n",
       "[2,]  1.08677588 -0.3911602  0.40524185  0.2449156\n",
       "[3,] -0.05750143 -0.1445250  0.27654390  0.3725861\n",
       "[4,]  0.34273597  1.5545848 -0.81929860 -0.1392084"
      ]
     },
     "metadata": {},
     "output_type": "display_data"
    }
   ],
   "source": [
    "m <- matrix(rnorm(16),4,4)\n",
    "m1 <- makeCacheMatrix(m)\n",
    "cacheSolve(m1)"
   ]
  },
  {
   "cell_type": "markdown",
   "metadata": {},
   "source": [
    "## Caching the Inverse of a Matrix: Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly.\n",
    "## Below are a pair of functions that are used to create a special object that stores a matrix and caches its inverse. This function creates a special \"matrix\" object that can cache its inverse."
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 8,
   "metadata": {},
   "outputs": [],
   "source": [
    "makeCacheMatrix <- function(x = matrix()) {\n",
    "inv <- NULL\n",
    "set <- function(y) {\n",
    "x <<- y\n",
    "inv <<- NULL\n",
    "}\n",
    "get <- function() x\n",
    "setInverse <- function(inverse) inv <<- inverse\n",
    "getInverse <- function() inv\n",
    "list(set = set,\n",
    "get = get,\n",
    "setInverse = setInverse,\n",
    "getInverse = getInverse)\n",
    "}"
   ]
  },
  {
   "cell_type": "markdown",
   "metadata": {},
   "source": [
    "## This function computes the inverse of the special \"matrix\" created by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then it should retrieve the inverse from the cache."
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 9,
   "metadata": {},
   "outputs": [],
   "source": [
    "cacheSolve <- function(x, ...) {\n",
    "## Return a matrix that is the inverse of 'x'\n",
    "inv <- x$getInverse()\n",
    "if (!is.null(inv)) {\n",
    "message(\"getting cached data\")\n",
    "return(inv)\n",
    "}\n",
    "mat <- x$get()\n",
    "inv <- solve(mat, ...)\n",
    "x$setInverse(inv)\n",
    "inv\n",
    "}"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": null,
   "metadata": {},
   "outputs": [],
   "source": [
    "# Testing\n",
    "source(\"ProgrammingAssignment2/cachematrix.R\")"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 11,
   "metadata": {},
   "outputs": [
    {
     "data": {
      "text/html": [
       "<table>\n",
       "<caption>A matrix: 2 × 2 of type int</caption>\n",
       "<tbody>\n",
       "\t<tr><td>1</td><td>3</td></tr>\n",
       "\t<tr><td>2</td><td>4</td></tr>\n",
       "</tbody>\n",
       "</table>\n"
      ],
      "text/latex": [
       "A matrix: 2 × 2 of type int\n",
       "\\begin{tabular}{ll}\n",
       "\t 1 & 3\\\\\n",
       "\t 2 & 4\\\\\n",
       "\\end{tabular}\n"
      ],
      "text/markdown": [
       "\n",
       "A matrix: 2 × 2 of type int\n",
       "\n",
       "| 1 | 3 |\n",
       "| 2 | 4 |\n",
       "\n"
      ],
      "text/plain": [
       "     [,1] [,2]\n",
       "[1,] 1    3   \n",
       "[2,] 2    4   "
      ]
     },
     "metadata": {},
     "output_type": "display_data"
    }
   ],
   "source": [
    "my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))\n",
    "my_matrix$get()"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 13,
   "metadata": {},
   "outputs": [
    {
     "data": {
      "text/plain": [
       "NULL"
      ]
     },
     "metadata": {},
     "output_type": "display_data"
    }
   ],
   "source": [
    "my_matrix$getInverse()\n"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 14,
   "metadata": {},
   "outputs": [
    {
     "data": {
      "text/html": [
       "<table>\n",
       "<caption>A matrix: 2 × 2 of type dbl</caption>\n",
       "<tbody>\n",
       "\t<tr><td>-2</td><td> 1.5</td></tr>\n",
       "\t<tr><td> 1</td><td>-0.5</td></tr>\n",
       "</tbody>\n",
       "</table>\n"
      ],
      "text/latex": [
       "A matrix: 2 × 2 of type dbl\n",
       "\\begin{tabular}{ll}\n",
       "\t -2 &  1.5\\\\\n",
       "\t  1 & -0.5\\\\\n",
       "\\end{tabular}\n"
      ],
      "text/markdown": [
       "\n",
       "A matrix: 2 × 2 of type dbl\n",
       "\n",
       "| -2 |  1.5 |\n",
       "|  1 | -0.5 |\n",
       "\n"
      ],
      "text/plain": [
       "     [,1] [,2]\n",
       "[1,] -2    1.5\n",
       "[2,]  1   -0.5"
      ]
     },
     "metadata": {},
     "output_type": "display_data"
    }
   ],
   "source": [
    "cacheSolve(my_matrix)"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 16,
   "metadata": {},
   "outputs": [
    {
     "data": {
      "text/html": [
       "<table>\n",
       "<caption>A matrix: 2 × 2 of type dbl</caption>\n",
       "<tbody>\n",
       "\t<tr><td>-2</td><td> 1.5</td></tr>\n",
       "\t<tr><td> 1</td><td>-0.5</td></tr>\n",
       "</tbody>\n",
       "</table>\n"
      ],
      "text/latex": [
       "A matrix: 2 × 2 of type dbl\n",
       "\\begin{tabular}{ll}\n",
       "\t -2 &  1.5\\\\\n",
       "\t  1 & -0.5\\\\\n",
       "\\end{tabular}\n"
      ],
      "text/markdown": [
       "\n",
       "A matrix: 2 × 2 of type dbl\n",
       "\n",
       "| -2 |  1.5 |\n",
       "|  1 | -0.5 |\n",
       "\n"
      ],
      "text/plain": [
       "     [,1] [,2]\n",
       "[1,] -2    1.5\n",
       "[2,]  1   -0.5"
      ]
     },
     "metadata": {},
     "output_type": "display_data"
    }
   ],
   "source": [
    "\n",
    "my_matrix$getInverse()"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 17,
   "metadata": {},
   "outputs": [],
   "source": [
    "my_matrix$set(matrix(c(2, 2, 1, 4), 2, 2))"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 18,
   "metadata": {},
   "outputs": [
    {
     "data": {
      "text/html": [
       "<table>\n",
       "<caption>A matrix: 2 × 2 of type dbl</caption>\n",
       "<tbody>\n",
       "\t<tr><td>2</td><td>1</td></tr>\n",
       "\t<tr><td>2</td><td>4</td></tr>\n",
       "</tbody>\n",
       "</table>\n"
      ],
      "text/latex": [
       "A matrix: 2 × 2 of type dbl\n",
       "\\begin{tabular}{ll}\n",
       "\t 2 & 1\\\\\n",
       "\t 2 & 4\\\\\n",
       "\\end{tabular}\n"
      ],
      "text/markdown": [
       "\n",
       "A matrix: 2 × 2 of type dbl\n",
       "\n",
       "| 2 | 1 |\n",
       "| 2 | 4 |\n",
       "\n"
      ],
      "text/plain": [
       "     [,1] [,2]\n",
       "[1,] 2    1   \n",
       "[2,] 2    4   "
      ]
     },
     "metadata": {},
     "output_type": "display_data"
    }
   ],
   "source": [
    "my_matrix$get()"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 19,
   "metadata": {},
   "outputs": [
    {
     "data": {
      "text/plain": [
       "NULL"
      ]
     },
     "metadata": {},
     "output_type": "display_data"
    }
   ],
   "source": [
    "my_matrix$getInverse()\n"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 20,
   "metadata": {},
   "outputs": [
    {
     "data": {
      "text/html": [
       "<table>\n",
       "<caption>A matrix: 2 × 2 of type dbl</caption>\n",
       "<tbody>\n",
       "\t<tr><td> 0.6666667</td><td>-0.1666667</td></tr>\n",
       "\t<tr><td>-0.3333333</td><td> 0.3333333</td></tr>\n",
       "</tbody>\n",
       "</table>\n"
      ],
      "text/latex": [
       "A matrix: 2 × 2 of type dbl\n",
       "\\begin{tabular}{ll}\n",
       "\t  0.6666667 & -0.1666667\\\\\n",
       "\t -0.3333333 &  0.3333333\\\\\n",
       "\\end{tabular}\n"
      ],
      "text/markdown": [
       "\n",
       "A matrix: 2 × 2 of type dbl\n",
       "\n",
       "|  0.6666667 | -0.1666667 |\n",
       "| -0.3333333 |  0.3333333 |\n",
       "\n"
      ],
      "text/plain": [
       "     [,1]       [,2]      \n",
       "[1,]  0.6666667 -0.1666667\n",
       "[2,] -0.3333333  0.3333333"
      ]
     },
     "metadata": {},
     "output_type": "display_data"
    }
   ],
   "source": [
    "cacheSolve(my_matrix)"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 22,
   "metadata": {},
   "outputs": [
    {
     "name": "stderr",
     "output_type": "stream",
     "text": [
      "getting cached data\n",
      "\n"
     ]
    },
    {
     "data": {
      "text/html": [
       "<table>\n",
       "<caption>A matrix: 2 × 2 of type dbl</caption>\n",
       "<tbody>\n",
       "\t<tr><td> 0.6666667</td><td>-0.1666667</td></tr>\n",
       "\t<tr><td>-0.3333333</td><td> 0.3333333</td></tr>\n",
       "</tbody>\n",
       "</table>\n"
      ],
      "text/latex": [
       "A matrix: 2 × 2 of type dbl\n",
       "\\begin{tabular}{ll}\n",
       "\t  0.6666667 & -0.1666667\\\\\n",
       "\t -0.3333333 &  0.3333333\\\\\n",
       "\\end{tabular}\n"
      ],
      "text/markdown": [
       "\n",
       "A matrix: 2 × 2 of type dbl\n",
       "\n",
       "|  0.6666667 | -0.1666667 |\n",
       "| -0.3333333 |  0.3333333 |\n",
       "\n"
      ],
      "text/plain": [
       "     [,1]       [,2]      \n",
       "[1,]  0.6666667 -0.1666667\n",
       "[2,] -0.3333333  0.3333333"
      ]
     },
     "metadata": {},
     "output_type": "display_data"
    }
   ],
   "source": [
    "cacheSolve(my_matrix)\n"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 23,
   "metadata": {},
   "outputs": [
    {
     "data": {
      "text/html": [
       "<table>\n",
       "<caption>A matrix: 2 × 2 of type dbl</caption>\n",
       "<tbody>\n",
       "\t<tr><td> 0.6666667</td><td>-0.1666667</td></tr>\n",
       "\t<tr><td>-0.3333333</td><td> 0.3333333</td></tr>\n",
       "</tbody>\n",
       "</table>\n"
      ],
      "text/latex": [
       "A matrix: 2 × 2 of type dbl\n",
       "\\begin{tabular}{ll}\n",
       "\t  0.6666667 & -0.1666667\\\\\n",
       "\t -0.3333333 &  0.3333333\\\\\n",
       "\\end{tabular}\n"
      ],
      "text/markdown": [
       "\n",
       "A matrix: 2 × 2 of type dbl\n",
       "\n",
       "|  0.6666667 | -0.1666667 |\n",
       "| -0.3333333 |  0.3333333 |\n",
       "\n"
      ],
      "text/plain": [
       "     [,1]       [,2]      \n",
       "[1,]  0.6666667 -0.1666667\n",
       "[2,] -0.3333333  0.3333333"
      ]
     },
     "metadata": {},
     "output_type": "display_data"
    }
   ],
   "source": [
    "my_matrix$getInverse()"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": null,
   "metadata": {},
   "outputs": [],
   "source": []
  }
 ],
 "metadata": {
  "kernelspec": {
   "display_name": "R",
   "language": "R",
   "name": "ir"
  },
  "language_info": {
   "codemirror_mode": "r",
   "file_extension": ".r",
   "mimetype": "text/x-r-source",
   "name": "R",
   "pygments_lexer": "r",
   "version": "3.6.3"
  }
 },
 "nbformat": 4,
 "nbformat_minor": 4
}
