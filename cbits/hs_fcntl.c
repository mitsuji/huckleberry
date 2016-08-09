#include "hs_fcntl.h"

#include <sys/fcntl.h>


int o_rdonly() {
  return O_RDONLY;
}
int o_wronly() {
  return O_WRONLY;
}
int o_rdwr() {
  return O_RDWR;
}

