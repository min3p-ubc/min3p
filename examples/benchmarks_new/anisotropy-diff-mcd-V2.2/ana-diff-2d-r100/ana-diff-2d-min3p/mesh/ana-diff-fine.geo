//+
Point(1) = {-1, -1, 0, 0.02828};
//+
Point(2) = {1, -1, 0, 0.02828};
//+
Point(3) = {1, 1, 0, 0.02828};
//+
Point(4) = {-1, 1, 0, 0.02828};
//+
Point(5) = {-0.12, -0.12, 0, 0.02828};
//+
Point(6) = {0.12, -0.12, 0, 0.02828};
//+
Point(7) = {0.12, 0.12, 0, 0.02828};
//+
Point(8) = {-0.12, 0.12, 0, 0.02828};
//+
Point(9) = {-0.10, 0.10, 0, 0.02828};
//+
Point(10) = {-0.10, -0.10, 0, 0.02828};
//+
Point(11) = {0.10, -0.10, 0, 0.02828};
//+
Point(12) = {0.10, 0.10, 0, 0.02828};
//+
Point(13) = {0.0, 0, 0, 0.02828};
//+
Point(14) = {0.2, 0, 0, 0.02828};
//+
Point(15) = {0.4, 0, 0, 0.02828};
//+
Point(16) = {0.0, 0.2, 0, 0.02828};
//+
Point(17) = {0.0, 0.4, 0, 0.02828};
//+
Point(18) = {0.2, 0.2, 0, 0.02828};
//+
Point(19) = {0.4, 0.4, 0, 0.02828};
//+
Line(1) = {1, 2};
//+
Line(2) = {2, 3};
//+
Line(3) = {3, 4};
//+
Line(4) = {4, 1};
//+
Line(5) = {5, 6};
//+
Line(6) = {6, 7};
//+
Line(7) = {7, 8};
//+
Line(8) = {8, 5};
//+
Line(9) = {10, 11};
//+
Line(10) = {11, 12};
//+
Line(11) = {12, 9};
//+
Line(12) = {9, 10};

//+
Curve Loop(1) = {4, 1, 2, 3};
//+
Plane Surface(1) = {1};
//+
Line{5} In Surface{1};
//+
Line{6} In Surface{1};
//+
Line{7} In Surface{1};
//+
Line{8} In Surface{1};
//+
Line{9} In Surface{1};
//+
Line{10} In Surface{1};
//+
Line{11} In Surface{1};
//+
Line{12} In Surface{1};
//+
Point{13} In Surface{1};
//+
Point{14} In Surface{1};
//+
Point{15} In Surface{1};
//+
Point{16} In Surface{1};
//+
Point{17} In Surface{1};
//+
Point{18} In Surface{1};
//+
Point{19} In Surface{1};


//+
Field[1] = Box;
//+
Field[1].VIn = 0.005;
//+
Field[1].VOut = 0.02828;
//+
Field[1].XMax = 0.4;
//+
Field[1].XMin = -0.4;
//+
Field[1].YMax = 0.4;
//+
Field[1].YMin = -0.4;
//+
Background Field = 1;
//+
Field[1].XMax = 0.5;
//+
Field[1].XMin = -0.5;
//+
Field[1].YMax = 0.5;
//+
Field[1].YMin = -0.5;
