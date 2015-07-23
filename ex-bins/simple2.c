int main(int argc, char **argv) {
  int x;
  if (argc&1) { x = (((argc|5)+8)|23)+1; }
  else { x = (argc+4)&0x77; }
  return (*argv) = x << 3;
}
