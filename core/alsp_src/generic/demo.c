#ifdef DEMO
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <time.h>

static long calculate_checksum(long n)
{
	long s, d1, d2, d3, d4;
		
	s = n + 247;
	
	d1 = (s % 10)/1;
	d2 = (s % 100)/10;
	d3 = (s % 1000)/100;
	d4 = (s % 10000)/1000;
	
	s = d3*1000 + d1*100 + d2*10 + d4;
	
	s = s * 5;
	
	s = s - n * 3;
	
	if (s < 0) s = -s;
	
	s = s % 10000;
	
	return s;
}


#ifdef macintosh

FSSpec DemoFileSpec;

static void init_demo_key_info(void)
{
	OSErr err;
	short vref;
	long dirid;
	
	err = FindFolder(kOnSystemDisk, kPreferencesFolderType,
					 kDontCreateFolder, &vref, &dirid);
	if (err == noErr) {
		FSMakeFSSpec(vref, dirid, "\pALS Prolog Demo Key", &DemoFileSpec);
	}
}

static int demo_key_info_exists(void)
{
	FInfo info;
	return (FSpGetFInfo(&DemoFileSpec, &info) == noErr);
}

static void read_demo_key_info(char *key, int max)
{
	OSErr err;
	short f;
	long count;
	
	err = FSpOpenDF(&DemoFileSpec, fsRdPerm, &f);
	if (err == noErr) {
		count = max-1;
		err = FSRead(f, &count, key);
		if (err == noErr || err == eofErr) key[count] = 0;
		else key[0] = 0;
		FSClose(f);
	}	
}

static void write_demo_key_info(char *key)
{
	OSErr err;
	short f;
	long count;
	
	err = FSpCreate(&DemoFileSpec, '????', '????', smSystemScript);
	if (err == noErr) {
		err = FSpOpenDF(&DemoFileSpec, fsRdWrPerm, &f);
		if (err == noErr) {
			count = strlen(key);
			FSWrite(f, &count, key);
			FSClose(f);
		}
	}
}

static void delete_demo_key_info(void)
{
	OSErr err;
	err = FSpDelete(&DemoFileSpec);
}
#endif

#ifdef WIN32

#define ALS_SUB_KEY "SOFTWARE\\Applied Logic Systems\\ALS Prolog"
#define DEMO_REG_KEY "Demo Key"

static void init_demo_key_info(void)
{
}

static int demo_key_info_exists(void)
{
	HKEY reg_key;
	LONG err;
	
	err = RegOpenKeyEx(HKEY_CURRENT_USER, ALS_SUB_KEY, 0, KEY_READ, &reg_key);
	if (err == ERROR_SUCCESS) {
		err = RegQueryValueEx(reg_key, DEMO_REG_KEY, NULL, NULL, NULL, NULL); 
		RegCloseKey(&reg_key);
	}
	
	return (err == ERROR_SUCCESS);
}

static void read_demo_key_info(char *key, int max)
{
	HKEY reg_key;
	LONG err;
	DWORD type, size;
	
	err = RegOpenKeyEx(HKEY_CURRENT_USER, ALS_SUB_KEY, 0, KEY_READ, &reg_key);
	if (err == ERROR_SUCCESS) {
		size = max;
		err = RegQueryValueEx(reg_key, DEMO_REG_KEY, NULL, &type, key, &size); 
		if (err != ERROR_SUCCESS || type != REG_SZ) {
			key[0] = 0;
		}
		RegCloseKey(&reg_key);
	}
}

static void write_demo_key_info(char *key)
{
	HKEY reg_key;
	LONG err;
	DWORD disposition;
		
	err = RegCreateKeyEx(HKEY_CURRENT_USER, ALS_SUB_KEY, 0, "", REG_OPTION_NON_VOLATILE, 
				KEY_WRITE, NULL, &reg_key, &disposition);
	if (err == ERROR_SUCCESS) {
		RegSetValueEx(reg_key, DEMO_REG_KEY, 0, REG_SZ, key, strlen(key)+1); 
		RegCloseKey(&reg_key);
	}
}

static void delete_demo_key_info(void)
{
	HKEY reg_key;
	LONG err;
	
	err = RegOpenKeyEx(HKEY_CURRENT_USER, ALS_SUB_KEY, 0, KEY_WRITE, &reg_key);
	if (err == ERROR_SUCCESS) {
		RegDeleteValue(reg_key, DEMO_REG_KEY); 
		RegCloseKey(&reg_key);
	}
}
#endif

#ifdef UNIX
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

char *demo_file_path;
#define DEMO_FILE ".alspro_demo_key"

static void init_demo_key_info(void)
{
  const char *home;
	
  home = getenv("HOME");
  if (home) {
    demo_file_path = malloc(strlen(home)+1+strlen(DEMO_FILE)+1);
    strcpy(demo_file_path, home);
    strcat(demo_file_path, "/");
    strcat(demo_file_path, DEMO_FILE);
  } else {
    demo_file_path = malloc(strlen(DEMO_FILE)+1);
    strcpy(demo_file_path, DEMO_FILE);
  }
}

static int demo_key_info_exists(void)
{
	return !access(demo_file_path, R_OK);
}

static void read_demo_key_info(char *key, int max)
{
	int f;
	size_t count;

	f = open(demo_file_path, O_RDONLY);
	
	if (f != -1) {
		count = read(f, key, max-1);
		if (count != -1) key[count] = 0;
		else key[0] = 0;
		close(f); 
	}
}

static void write_demo_key_info(char *key)
{
	int f;
	
	f = creat(demo_file_path, S_IRUSR | S_IWUSR);
	if (f != -1) {
		write(f, key, strlen(key));
		close(f);
	}
}

static void delete_demo_key_info(void)
{
	unlink(demo_file_path);
}
#endif

static void console_get_demo_key(char *key, int max)
{
	fprintf(stdout, "\
ALS Prolog Evaluation System\n\
\n\
This evaluation copy requires a serial number key in order to run.\n\
If you do not have a key, please contact Applied Logic Systems to\n\
receive an evaluation key.\n\
\n\
Web    http://www.als.com\n\
E-Mail info@als.com\n\
Voice  +1 (617) 497-0100\n\
Fax    +1 (617) 497-3963\n\
Mail   Applied Logic Systems, Inc.\n\
       P.O. Box 175\n\
       Cambridge, MA 02140\n\
       USA\n\
\n\
");
	fprintf(stdout, "Please enter the evaluation key: ");
	fflush(stdout);
	fgets(key, max, stdin);
	if (key[strlen(key)-1] == '\n') key[strlen(key)-1] = 0;
	fprintf(stdout, "\n");
}

static void console_demo_error(const char *s)
{
	fprintf(stderr, "%s\n", s);
	exit(EXIT_FAILURE);
}

void (*demo_error)(const char *s) = console_demo_error;
void (*get_demo_key)(char *key, int max) = console_get_demo_key;

#define KEY_MAX 40

static int rotate(int n, int r)
{
	int n1,n2,n3,n4,r1,r2,r3,r4;
	
	n1 = (n % 10)/1;
	n2 = (n % 100)/10;
	n3 = (n % 1000)/100;
	n4 = (n % 10000)/1000;

	r1 = (r % 10)/1;
	r2 = (r % 100)/10;
	r3 = (r % 1000)/100;
	r4 = (r % 10000)/1000;
	
	n1 = (n1 + r1) % 10;
	n2 = (n2 + r2) % 10;
	n3 = (n3 + r3) % 10;
	n4 = (n4 + r4) % 10;
	
	return (n1 + n2*10 + n3*100 + n4*1000);
}

static int valid_demo_key(const char *key)
{
	int a,b,c,d, duration;
	struct tm start_tm = {0};
	time_t start_time;
	double diff;
	
	sscanf(key, "%d-%d-%d-%d", &a, &b, &c, &d);

	if (d != calculate_checksum(a ^ b ^ c)) return 0;

	a = rotate(a, 4935);
	b = rotate(b, 6723);
	c = rotate(c, 2385); 
	

	start_tm.tm_mon = (a/100)%100 - 1;
	start_tm.tm_mday = a%100;
	start_tm.tm_year = (b/100)%100 + 98;
	
	start_time = mktime(&start_tm);
	
	if (start_time == (time_t) -1) return 0;

	duration = b%100;
	
	diff = difftime(time(NULL), start_time);
	
	return diff > 0.0 && diff < duration * 24.0 * 60.0 * 60.0;
}

void demo_check(void);
void demo_check(void)
{
	char key[KEY_MAX];
	
	init_demo_key_info();
	
	if (demo_key_info_exists()) {
		read_demo_key_info(key, KEY_MAX);
		if (!valid_demo_key(key)) {
			delete_demo_key_info();
			demo_error("The evaluation key has expired.");
		}
	} else {
		get_demo_key(key, KEY_MAX);
		if (!valid_demo_key(key)) {
			demo_error("Invalid evaluation key.");
		}
		write_demo_key_info(key);
	}
}
#endif /* DEMO */
