{ def tak(x: Int, y: Int, z:Int): Int =
    if (x <= y) y
    else tak(tak(x-1, y, z), tak(y-1, z, x), tak(z-1, x, y));
  tak(n, 0, n+1)
}
