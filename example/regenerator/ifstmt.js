function* foo(p, x, t, f) {
  if (p(x)) {
    bar(t);
    baz(f);
    yield t;
  } else {
    yield f;
  }

  return 0;
}
