static int copy_file(const char *from_file, const char *to_file)
{
    if (CopyFile(from_file, to_file, FALSE)) return 1;
    else return 0;
}
