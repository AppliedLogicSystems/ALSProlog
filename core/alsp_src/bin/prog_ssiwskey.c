/*

Compile like this:

% gcc -o prog_ssiwkey prog_ssiwskey.c /usr/local/lib/*.o -lsocket


*/

#include <stdio.h>
#include <stdlib.h>

#include <netinet/in.h>

#include <unix_e.h>

BYTE _SSIWS_PrivateCompIdCodes[] =
{
0x01, 0xd4, 0xc8, 0x43, 0xe3, 0xad, 0x8a, 0xe7, 0xeb, 0x13, 0xe3, 0x14, 0x81,
0xc6, 0xfe, 0x70, 0x7b, 0x1f, 0x0b, 0x50, 0x8d, 0xe7, 0xc0, 0x4e, 0xa6, 0x91,
0x7b, 0x64, 0x48, 0xd4
};

#define SSIWS_APPLICATION_ID    0x948576

static void error(const char *message)
{
    fprintf(stderr, "SSIWS Key Programmer Error:\n  %s\n", message);
    exit(EXIT_FAILURE);
}

typedef enum {simple_key, timed_key} key_type;

#define mem_max 4

static void program_key(key_type type, const char *device)
{
    unsigned long mem_init[mem_max] = {
	0, 0, 0, 0
    };
    
    if (SSIWS_Initialize(device)) error("SSIWS_Initialize() failed.");
    
    
    if (type == timed_key) {
    	mem_init[0] = htonl(1);
    	mem_init[3] = htonl(31*24*60*60); /* one month in seconds */
    }
    
    if (SSIWS_WriteMemory(SSIWS_APPLICATION_ID, 0, (WORD *)mem_init, mem_max*2))
    	error("SSIWS_WriteMemory() failed.");
    
    if (SSIWS_ShutDown()) error("SSIWS_ShutDown() failed.");
}


static void usage(void)
{
    fprintf(stderr, "Usage: prog_ssiwskey (-simple | -timed) device\n");
}

int main(int argc, char **argv)
{
    key_type type;
    
    if (argc != 3) {
    	fprintf(stderr, "Too few/many options.\n");
    	usage();
    
        return EXIT_FAILURE;
    }
    
    if (strcmp(argv[1], "-simple") == 0) type = simple_key;
    else if (strcmp(argv[1], "-timed") == 0) type = timed_key;
    else {
        fprintf(stderr, "Unknown option: %s\n", argv[1]);
        usage();
        return EXIT_FAILURE;    
    }
    
    program_key(type, argv[2]);
    
    printf("Key programmed successfully.\n");
    
    return EXIT_SUCCESS;
}
