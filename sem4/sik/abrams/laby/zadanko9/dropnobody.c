// dropping root priviledges; see
// http://stackoverflow.com/questions/3357737/dropping-root-privileges


#include <pwd.h>
#include <stdlib.h>
#include <sys/types.h>
#include <unistd.h>

#include "err.h"

void drop_to_nobody() {
  
  struct passwd * nobody_passwd;

  nobody_passwd = getpwnam("nobody");
  if (nobody_passwd  == NULL) 
    syserr("getpwnam");
  
  if (setgid(nobody_passwd -> pw_gid) != 0)
    syserr("setgid");
  if (setuid(nobody_passwd -> pw_uid) != 0)
    syserr("setuid");
  
   if (setuid(0) != -1)
     fatal("ERROR: Managed to regain root privileges?");

}
