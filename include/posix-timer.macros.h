#ifndef POSIX_TIMER_MACROS_H
#define POSIX_TIMER_MACROS_H

#define hsc_itype(t) \
  printf("%s%lu", ((t)(-1)) < 0 ? "Int" : "Word", sizeof (t) * 8);

#endif /* POSIX_TIMER_MACROS_H */

