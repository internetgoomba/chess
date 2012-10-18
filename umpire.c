#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* other state things may be needed!! eg. has castles, has been in
   check, moves since last pawn move */
/* row and col 0 are not used (chess naming is 1 to 8) */
int board [9] [9];

/* white & black programs */
char white_p [32];
char black_p [32];

int play_game (void);
int legal (char *move, int player);
int draw_reached (void);
void get_move (char *program, int w, char *move);
int in_check (int player);
int piece_at (int r, int c);
int color_at (int r, int c);
void update_board (char *move);
void new_board (void);
int start_row (char *move);
int start_col (char *move);
int end_row (char *move);
int end_col (char *move);
void print_board (FILE *f);

int 
main (int argc, char **argv)
{
  if (argc != 3)
    return 1;

  /* read program names */
  strcpy (white_p, argv [1]);
  strcpy (black_p, argv [2]);

  new_board ();
  int result = play_game ();
  switch (result)
    {
    case 0:
      printf ("White wins\n");
      break;
    case 1:
      printf ("Black wins\n");
      break;
    case 2:
      printf ("Draw\n");
      break;
    }
  return 0;
}

/* 0 = w win, 1 = b win, 2 = draw */
int
play_game (void)
{
  char move [10];
  /* 0 = white, 1 = black */
  int player = 0;

  while (1)
    {
      print_board (stdout);
      (player == 0) ? get_move (white_p, 0, move) : get_move (black_p, 1, move);
      
      if (!legal (move, player))
        return !player;
      
      update_board (move);
      if (in_check (player))
	return !player;

      if (draw_reached ())
	return 2;

      player = !player;
    }
}

#define EMPTY 0x0
#define PAWN 0x1
#define BISHOP 0x2
#define KNIGHT 0x3
#define ROOK 0x4
#define QUEEN 0x5
#define KING 0x6

void
print_board (FILE *f)
{
  int i, j;
  for (j = 0; j < 10; j++)
    fprintf (f, "-");
  fprintf (f, "\n");
  for (i = 8; i >= 1; i--)
    {
      fprintf (f, "|");
      for (j = 1; j <= 8; j++)
	switch (piece_at (i, j))
	  {
	  case EMPTY:
	    fprintf (f, " "); break;
	  case PAWN:
	    if (color_at (i, j) == 0)
	      fprintf (f, "P"); 
	    else
	      fprintf (f, "p");
	    break;
	  case KNIGHT:
	    if (color_at (i, j) == 0)
	      fprintf (f, "N"); 
	    else
	      fprintf (f, "n");
	    break;
	  case BISHOP:
	    if (color_at (i, j) == 0)
	      fprintf (f, "B"); 
	    else
	      fprintf (f, "b");
	    break;
	  case ROOK:
	    if (color_at (i, j) == 0)
	      fprintf (f, "R"); 
	    else
	      fprintf (f, "r");
	    break;
	  case QUEEN:
	    if (color_at (i, j) == 0)
	      fprintf (f, "Q"); 
	    else
	      fprintf (f, "q");
	    break;
	  case KING:
	    if (color_at (i, j) == 0)
	      fprintf (f, "K"); 
	    else
	      fprintf (f, "k");
	    break;
	  }
      fprintf (f, "|\n");
    }
  for (j = 0; j < 10; j++)
    fprintf (f, "-");
  fprintf (f, "\n");
}

int
start_row (char *move)
{
  return (move [1] - '0');
}

int
start_col (char *move)
{
  return (move [0] - 'a' + 1);
}

int
end_row (char *move)
{
  return (move [5] - '0');
}

int
end_col (char *move)
{
  return (move [4] - 'a' + 1);
}

int 
piece_at (int r, int c)
{
  return (board [r] [c] & 0x7);
}

int 
color_at (int r, int c)
{
  return ((board [r] [c] & 0x8) == 0x8);
}

void 
update_board (char *move)
{
  board [end_row (move)] [end_col (move)] = board [start_row (move)] [start_col (move)];
  board [start_row (move)] [start_col (move)] = EMPTY;
}

void 
new_board (void)
{
  int r, c;
  for (r = 3; r <= 6; r++)
    for (c = 1; c <= 8; c++)
      board [r] [c] = EMPTY;

  for (c = 1; c <= 8; c++)
    {
      board [2] [c] = PAWN;
      board [7] [c] = PAWN | 0x8;
    }
  board [1] [1] = ROOK;
  board [1] [2] = KNIGHT;
  board [1] [3] = BISHOP;
  board [1] [4] = QUEEN;
  board [1] [5] = KING;
  board [1] [6] = BISHOP;
  board [1] [7] = KNIGHT;
  board [1] [8] = ROOK;
  
  board [8] [1] = ROOK | 0x8;
  board [8] [2] = KNIGHT | 0x8;
  board [8] [3] = BISHOP | 0x8;
  board [8] [4] = QUEEN | 0x8;
  board [8] [5] = KING | 0x8;
  board [8] [6] = BISHOP | 0x8;
  board [8] [7] = KNIGHT | 0x8;
  board [8] [8] = ROOK | 0x8;
}

void
get_move (char *prog, int w, char *move)
{
  /* pass the program the board state, wait 30 secs for it to return a move */
  char message [50];
  FILE *f = fopen ("./.chess.log", "w");
  print_board (f);
  fprintf (f, "%c\n", ((w == 0) ? 'w' : 'b'));
  fclose (f);
  system (prog);

  /* needs to be made to read what was previously on stdout.  Change
     stdout to stdin to test manually */
  f = fopen ("./.chess.log", "r");
  fgets (move, 10, f);
  fclose (f);
}

/* the two big fns */
int
draw_reached (void)
{
  /* todo */
  return 0;
}

int
legal (char *move, int player)
{
  /* need to add castling */

  int s_row = start_row (move);
  int s_col = start_col (move);
  int e_row = end_row (move);
  int e_col = end_col (move);

  int dr, dc, i, j;

  /* must end on board */
  if ((e_row > 8) || (e_col > 8) || (e_row < 1) || (e_col < 1))
    return 0;

  /* piece must move */
  if ((s_row == e_row) && (s_col == e_col))
    return 0;

  /* can't take own piece */
  if ((piece_at (e_row, e_col) != EMPTY) 
      && (color_at (e_row, e_col) == player))
    return 0;

  switch (piece_at (s_row, s_col))
    {
    case EMPTY:
      /* Must move a piece! */
      return 0;

    case PAWN:
      /* pawns only go one way! */
      dr = (player == 0) ? 1 : -1;

      /* 1 step "forward" into empty square */
      if ((e_row == s_row + dr) && (e_col == s_col)
	  && (piece_at (e_row, e_col) == EMPTY))
	return 1;
      
      /* take diagonally */
      if ((e_row == s_row + dr) && abs (e_col - s_col) == 1
	  && (piece_at (e_row, e_col) != EMPTY))
	return 1;

      /* double step form row 2 if white, row 7 if black */
      int row2 = (player == 0) ? 4 : 5;
      if ((e_row == row2) && (e_col == s_col)
	  && (piece_at (e_row, e_col) == EMPTY) 
	  && (piece_at (s_row + dr, s_col) == EMPTY))
	return 1;

      /*Screw en-passant */
      return 0;

    case KNIGHT:
      /* two rows and 1 column, or two columns and 1 row */
      if ((abs (s_row - e_row) == 2 && abs (s_col - e_col) == 1) ||
	  (abs (s_row - e_row) == 1 && abs (s_col - e_col) == 2))
	return 1;
      return 0;

    case BISHOP:
      /* must be diagonal */
      if (abs (e_row - s_row) != abs (e_col - s_col))
	return 0;
      
      /* pieces can't be "teleported over" */
      dr = (e_row > s_row) ? 1 : -1;
      dc = (e_col > s_col) ? 1 : -1;
      for (i = s_row + dr, j = s_col + dc; i != e_row; i += dr, j += dc)
	  if (piece_at (i, j) != EMPTY)
	    return 0;
      return 1;

    case ROOK:
      /* must be along a row or column */
      if ((s_row != e_row) && (s_col != e_col))
	return 0;
      
      /* pieces can't be "teleported over" */
      dc = (s_col == e_col) ? 0 : ((e_col > s_col) ? 1 : -1);
      dr = (s_row == e_row) ? 0 : ((e_row > s_row) ? 1 : -1);
      if (dc == 0)
	{
	  for (i = s_row + dr; i != e_row; i += dr)
	    if (piece_at (i, s_col) != EMPTY)
	      return 0;
	}
      else if (dr == 0)
	{
	  for (i = s_col + dc; i != e_col; i += dc)
	    if (piece_at (s_row, i) != EMPTY)
	      return 0;
	}
      return 1;

    case QUEEN:
      /* diagonally, or along a row/col */
      if (abs (e_row - s_row) == abs (e_col - s_col))
	{
	  /* moving diagonally */
	  /* pieces can't be "teleported over" */
	  dr = (e_row > s_row) ? 1 : -1;
	  dc = (e_col > s_col) ? 1 : -1;
	  for (i = s_row + dr, j = s_col + dc; i != e_row; i += dr, j += dc)
	      if (piece_at (i, j) != EMPTY)
		return 0;
	  return 1;
	}
      else if ((s_row == e_row) || (s_col == e_col))
	{
	  /* moving along row/col */
	  /* pieces can't be "teleported over" */
	  dc = (s_col == e_col) ? 0 : ((e_col > s_col) ? 1 : -1);
	  dr = (s_row == e_row) ? 0 : ((e_row > s_row) ? 1 : -1);
	  if (dc == 0)
	    {
	      for (i = s_row + dr; i != e_row; i += dr)
		if (piece_at (i, s_col) != EMPTY)
		  return 0;
	    }
	  else if (dr == 0)
	    {
	      for (i = s_col + dc; i != e_col; i += dc)
		if (piece_at (s_row, i) != EMPTY)
		  return 0;
	    }
	  return 1;
	}
      return 0;
      
    case KING:
      if (abs (s_col - e_col) != 1 && (s_col != e_col))
	return 0;
      if (abs (s_row - e_row) != 1 && (s_row != e_row))
	return 0;
      return 1;
    }
}
 
int 
in_check (int player)
{
  int r, c, er, ec;
  char move [10];
  /* for all pieces */
  for (r = 1; r <= 8; r++)
    for (c = 1; c <= 8; c++)
      for (er = 1; er <= 8; er++)
	for (ec = 1; ec <= 8; ec++)
	  {
	    /* Dumb bug when I forgot this earlier! */
	    if (color_at (r, c) == player)
	      continue;

	    /* if it can legally take the king */
	    sprintf (move, "%c%d->%c%d", 'a' - 1 + c, r, 'a' -1 + ec, er);
	    if (legal (move, !player) && piece_at (er, ec) == KING 
		&& color_at (er, ec) == player)
	      return 1;
	  }
  return 0;
}
