#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "log.h"
#include "s7.h"
/* #include "s7_config.h" */
/* #include "mibl.h" */
#include "xen_repl.h"

extern s7_scheme *s7;

/* pilfered from https://ccrma.stanford.edu/software/snd/index.html */

/* io.c */
char *mus_strdup(const char *str)
{
  char *newstr = NULL;
  int len;
  if ((!str) || (!(*str))) return(NULL);
  len = strlen(str);
  newstr = (char *)malloc(len + 1);
  strcpy(newstr, str);
  newstr[len] = '\0';
  return(newstr);
}

int mus_strlen(const char *str)
{
  /* strlen(NULL) -> seg fault! */
  if ((str) && (*str)) return(strlen(str));
  return(0);
}

/* snd-xen.c */
static char *stdin_str = NULL;

void stdin_free_str(void)
{
  if (stdin_str) free(stdin_str);
  stdin_str = NULL;
}

static int check_balance(const char *expr, int start, int end) 
{
  int i;
  bool not_whitespace = false;
  int paren_count = 0;
  bool prev_separator = true;
  bool quote_wait = false;

  i = start;
  while (i < end) 
    {
      switch (expr[i]) 
	{
	case ';' :
	  /* skip till newline. */
	  do {
	    i++;
	  } while ((i < end) && (expr[i] != '\n'));
	  break;

	case ' ':
	case '\n':
	case '\t':
	case '\r':
	  if ((not_whitespace) && (paren_count == 0) && (!quote_wait))
	    return(i);
	  else 
	    {
	      prev_separator = true;
	      i++;
	    }
	  break;

	case '\"' :
	  if ((not_whitespace) && (paren_count == 0) && (!quote_wait))
	    return(i);
	  else 
	    {
	      /* skip past ", ignoring \", some cases:
	       *  "\"\"" '("\"\"") "\\" "#\\(" "'(\"#\\\")"
	       */
	      while (i < end)
		{
		  i++;
		  if (expr[i] == '\\') 
		    i++;
		  else
		    {
		      if (expr[i] == '\"')
			break;
		    }
		}
	      i++;
	      if (paren_count == 0) 
		{
		  if (i < end) 
		    return(i);
		  else return(0);
		} 
	      else 
		{
		  prev_separator = true;
		  not_whitespace = true;
		  quote_wait = false;
		}
	    }
	  break;

	case '#':
	  if ((i < end - 1) &&
	      (expr[i + 1] == '|'))
	    {
	      /* (+ #| a comment |# 2 1) */
	      i++;
	      do {
		i++;
	      } while (((expr[i] != '|') || (expr[i + 1] != '#')) && (i < end));
	      i++;
	      break;
	    }
	  else
	    {
	      /* (set! *#readers* (cons (cons #\c (lambda (str) (apply make-rectangular (read)))) *#readers*))
	       */
	      if ((not_whitespace) && (paren_count == 0) && (!quote_wait))
		return(i);
	      else 
		{
		  bool found_it = false;
		  if (prev_separator)
		    {
		      int k, incr = 0;
		      for (k = i + 1; k < end; k++)
			{
			  if (expr[k] == '(')
			    {
			      /* should we look at the readers here? I want to support #c(1 2) for example */
			      not_whitespace = false;
			      prev_separator = false;
			      incr = k - i;
			      break;
			    }
			  else
			    {
			      if ((!isdigit((int)expr[k])) && /* #2d(...)? */
				  (!isalpha((int)expr[k])) && /* #c(1 2)? */
				  (expr[k] != 'D') && 
				  (expr[k] != 'd') &&
				  (expr[k] != '=') &&   /* what is this for? */
				  (expr[k] != '#'))     /* perhaps #1d(#(1 2) 3) ? */
				break;
			    }
			}
		      if (incr > 0)
			{
			  i += incr;
			  found_it = true;
			}
		    }
		  if (!found_it)
		    {
		      if ((i + 2 < end) && (expr[i + 1] == '\\') && 
			  ((expr[i + 2] == ')') || (expr[i + 2] == ';') || (expr[i + 2] == '\"') || (expr[i + 2] == '(')))
			i += 3;
		      else
			{
			  prev_separator = false;
			  quote_wait = false;
			  not_whitespace = true;
			  i++;
			}
		    }
		}
	    }
	  break;

	case '(' :
	  if ((not_whitespace) && (paren_count == 0) && (!quote_wait))
	    return(i - 1); /* 'a(...) -- ignore the (...) */
	  else 
	    {
	      i++;
	      paren_count++;
	      not_whitespace = true;
	      prev_separator = true;
	      quote_wait = false;
	    }
	  break;

	case ')' :
	  paren_count--;
	  if ((not_whitespace) && (paren_count == 0))
	    return(i + 1);
	  else 
	    {
	      i++;
	      not_whitespace = true;
	      prev_separator = true;
	      quote_wait = false;
	    }
	  break;

	case '\'' :
	case '`' :                  /* `(1 2) */
	  if (prev_separator) 
	    quote_wait = true;
	  not_whitespace = true;
	  i++;
	  break;

	case ',':                   /* `,(+ 1 2) */
	case '@':                   /* `,@(list 1 2) */
	  prev_separator = false;
	  not_whitespace = true;
	  i++;
	  break;

	default:
	  prev_separator = false;
	  quote_wait = false;
	  not_whitespace = true;
	  i++;
	  break;
	}
    }

  return(0);
}

char *stdin_check_for_full_expression(const char *newstr)
{
    int end_of_text;
    if (stdin_str)
        {
            char *str;
            str = stdin_str;
            stdin_str = (char *)calloc(mus_strlen(str) + mus_strlen(newstr) + 2, sizeof(char));
            strcat(stdin_str, str);
            strcat(stdin_str, newstr);
            free(str);
        }
    else stdin_str = mus_strdup(newstr);
    /* #if HAVE_SCHEME */
    end_of_text = check_balance(stdin_str, 0, mus_strlen(stdin_str));
    if (end_of_text > 0)
        {
            if (end_of_text + 1 < mus_strlen(stdin_str))
                stdin_str[end_of_text + 1] = 0;
            return(stdin_str);
        }
    return(NULL);
    /* #endif */
    return(stdin_str);
}

/* from xen.c, s7 section: */
char *xen_strdup(const char *str)
{
  char *newstr = NULL;
  if ((!str) || (!(*str))) return(NULL);
  newstr = (char *)malloc(strlen(str) + 1);
  if (newstr) strcpy(newstr, str);
  return(newstr);
}

void xen_repl(int argc, char **argv)
{
  int size = 512;
  bool expr_ok = true;
  char *buffer;
  char *repl_prompt = xen_strdup("mibl> ");
  /* s7_pointer evalres; */
  buffer = (char *)calloc(size, sizeof(char));

  while (true)
    {
      if (expr_ok)
	{
	  fprintf(stderr, "\n%s", repl_prompt);
	  expr_ok = false; /* don't get into an infinite loop if running in the background! */
	}
      if (fgets(buffer, size, stdin))
	{
	  /* also, it's possible to get a string of spaces or nulls (? -- not sure what is coming in) if stdin is /dev/null */
	  /*   then if (as in condor) stdout is being saved in a file, we get in an infinite loop storing "snd>" until the disk fills up */
	  int i, len;

	  expr_ok = false;
	  len = strlen(buffer);
	  for (i = 0; i < len; i++)
	    {
	      if (buffer[i] == 0)
		break;
	      if (!isspace((int)buffer[i]))
		{
		  expr_ok = true;
		  break;
		}
	    }
	  if (expr_ok)
	    {
	      char *temp;
/* #if USE_SND */
	      char *str;
	      str = stdin_check_for_full_expression(buffer); /* "str" here is actually stdin_str, so we need to clear it explicitly */
	      if (!str) {expr_ok = false; continue;}
	      len = strlen(str) + 16;
	      temp = (char *)malloc(len * sizeof(char));
	      snprintf(temp, len, "(write %s)", str);
	      /* Xen_eval_C_string(temp); */

              // this writes result to stdout, somehow

	      /* evalres = */ //FIXME: check result
              s7_eval_c_string(s7, temp);

/* /\* The s7-built-in catch tags are 'wrong-type-arg, 'syntax-error, 'read-error, 'unbound-variable, 'out-of-memory, 'wrong-number-of-args, 'format-error, 'out-of-range, 'division-by-zero, 'io-error, and 'bignum-error. *\/ */
              /* log_debug("evalres: %s", TO_STR(evalres)); */
              /* log_debug("evalres t: %s", TO_STR(s7_type_of(s7, evalres))); */
              /* if (s7_ */
              fflush(stdout);
              fflush(stderr);
	      free(temp);
	      stdin_free_str();
/* #else */
/* 	      temp = (char *)malloc(len + 16); */
/* 	      snprintf(temp, len + 16, "(write %s)", buffer);    /\* use write, not display so that strings are in double quotes *\/ */
/* 	      /\* Xen_eval_C_string(temp); *\/ */
/* 	      s7_eval_c_string(s7, temp); */
/* 	      free(temp); */
/* #endif */
	    }
	}
    }
  /* unreachable */
  free(buffer);
}
