#include "defs.h"

extern const int is_shared_library;

#include <sys/param.h>

static char *which(const char *path_list, const char *name, char *path)
{
  const char *element, *next;
  char *result_path = NULL;
  
  for (element = path_list; element;
       element = next ? next+1 : NULL)
  {
    struct stat info;
    int length;

    next = strchr(element, ':');
    length = next ? next-element : strlen(element);
    strncpy(path, element, length);
    path[length] = 0;
    if (length > 0 && element[length] != '/') strcat(path, "/");
    strcat(path, name);

    if (access(path, X_OK) == 0
	&& stat(path, &info) == 0
	&& S_ISREG(info.st_mode)) {
      result_path = path;
      break;
    }
  }

  return result_path;
}

static void command_line_locate_executable(int argc, char *argv[])
{
  const char *exec_path = NULL;

  if (argc <= 0) {
    strcpy(executable_path, "alspro");
  } else {
    if (strchr(argv[0], '/')) {
      exec_path = argv[0];
    } else {
      const char *path_list;
      char path[MAXPATHLEN];

      path_list = getenv("PATH");
      if (!path_list) path_list = "/bin:/usr/bin:";

      exec_path = which(path_list, argv[0], path);
      if (!exec_path) exec_path = argv[0];
    }

    /* Cast exec_path to char *, because realpath isn't always defined
     with const char * */
    if (!realpath((char *)exec_path, executable_path)) {
      fatal_error(FE_INFND, 0);
      return;
    }
  }
}

static void locate_library(void)
{
  const char *path_list, *lib_path;
  char path[MAXPATHLEN];

  /* what to do about /etc/ld.so.cache? */
  /* What to do about -rdynamic paths? */

  path_list = getenv("LD_LIBRARY_PATH");
  if (!path_list) path_list = "/usr/lib:/lib";

  lib_path = which(path_list, "libalspro.so", path);
  if (!lib_path) lib_path = "libalspro.so";

  /* Cast to char *, because realpath isn't always defined
     with const char * */
  if (!realpath((char *)lib_path, library_path)) {
    fatal_error(FE_INFND, 0);
    return;
  }
}

void locate_library_executable(int argc, char *argv[])
{
    char *endpath;

    command_line_locate_executable(argc, argv);

    if (is_shared_library) {
      locate_library();
    } else strcpy(library_path, executable_path);

    strcpy(library_dir, library_path);

    endpath = strrchr(library_dir, '/');
    if (endpath == NULL) fatal_error(FE_INFND, 0);
    endpath++;  /* include the \ */
    *endpath = 0;
}


