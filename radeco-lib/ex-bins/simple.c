int main(int argc, char **argv) {
  if (argc&1) { return argc*argc; }
  else { return argc+4; }
}
