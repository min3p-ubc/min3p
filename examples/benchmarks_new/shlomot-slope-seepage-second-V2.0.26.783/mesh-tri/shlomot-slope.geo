// Gmsh project created on Thu Mar  5 20:28:37 2020
SetFactory("OpenCASCADE");
//+
Point(1) = {0, 0, 0, 0.0192};
//+
Point(2) = {0, 0, 0.192, 0.0192};
//+
Point(3) = {3, 0, 1.2, 0.05};
//+
Point(4) = {6, 0, 1.2, 0.05};
//+
Point(5) = {6, 0, 0, 0.05};
//+
Line(1) = {1, 2};
//+
Line(2) = {2, 3};
//+
Line(3) = {3, 4};
//+
Line(4) = {4, 5};
//+
Line(5) = {5, 1};
//+
Curve Loop(1) = {3, 4, 5, 1, 2};
//+
Plane Surface(1) = {1};
