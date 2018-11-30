/* GCC */
#include <iostream>
#include <cstdio>
#include <cstdlib>
#include <ctime>
#include <cmath>
#include <cstring>
#include <string>
#include <vector>
#include <list>
#include <map>
#include <set>
#include <algorithm>
#include <queue>
#include <sstream>

#define X first
#define Y second
#define mp(a, b) make_pair((a), (b))

using namespace std;

const pair <double, double> p_inf(-1e9, -1e9);

struct edge
{
    pair <double, double> A = p_inf;
    pair <double, double> B = p_inf;
    pair <double, double> n = p_inf;

    int a = -1;
    int b = -1;
};

vector <edge> edges;
vector < pair <double, double> > points;
pair <double, double> p1, p2, p3;
double x, y, xl, yl, a, b, c, d, e, f, g;
int n, i, j, num, na, nb, nc, nd, ne, sl;
priority_queue < pair <double, pair < pair <int, int>, pair <int, int> > > > ord, bad_ord;
pair < pair <int, int>, pair <int, int> > act;
list <int> line;
list <int> e_line;

pair <double, double> find_c(pair <double, double> p1, pair <double, double> p2, pair <double, double> p3)
{
    a = p2.X - p1.X;
    b = p2.Y - p1.Y;
    c = p3.X - p1.X;
    d = p3.Y - p1.Y;
    e = a * (p1.X + p2.X) + b * (p1.Y + p2.Y);
    f = c * (p1.X + p3.X) + d * (p1.Y + p3.Y);
    g = 2 * (a * (p3.Y - p2.Y) - b * (p3.X - p2.X));
    if(g == 0)
        return p_inf;

    return mp((d * e - b * f) / g, (a * f - c * e) / g);
}

pair <double, double> find_l(pair <double, double> p1, pair <double, double> p2, pair <double, double> p3)
{
    pair <double, double> p_c = find_c(p1, p2, p3);

    if(p_c == p_inf)
        return p_c;

    double R = sqrt((p_c.X - p1.X) * (p_c.X - p1.X) + (p_c.Y - p1.Y) * (p_c.Y - p1.Y));
    p_c.Y -= R;
    return p_c;
}

bool check(edge e, pair <double, double> p)
{
    double eps = 1e-9;

    double t1, t2, t;

    t = t1 = t2 = 0;

    p.X -= e.A.X;
    if(e.n.X == 0)
        t = -1;
    else
        t1 = p.X / e.n.X;

    p.Y -= e.A.Y;
    if(e.n.Y == 0)
        t = -2;
    else
        t2 = p.Y / e.n.Y;

    if(t == -1)
        t = t2;
    else if(t == -2)
        t = t1;
    else if(abs(t2 - t1) <= eps)
        t = t1;
    else
        return false;

    if(t >= 0)
        return true;
    else
        return false;
}

void first_act()
{
    /*for(auto it = line.begin(); it != line.end(); it++)
        cout << *it << " ";
    cout << endl;*/

    edge e, rev_e, e_p, e_n;

    na = act.X.Y;
    p2 = points[na];

    if(line.empty())
    {
        line.push_back(na);
        return;
    }

    xl = -1e9;
    auto it = line.begin();
    auto e_it_p = e_line.begin();
    auto e_it_n = e_line.begin();
    p1 = points[*it];

    while(e_it_n != e_line.end())
    {
        i = *it;
        j = *e_it_n;
        p1 = points[i];
        e = edges[j];

        a = e.n.X * e.n.X;
        b = 2 * (e.n.X * e.A.X - e.n.X * p1.X - e.n.Y * p1.Y + e.n.Y * yl);
        c = e.A.X * e.A.X + p1.X * p1.X + p1.Y * p1.Y - yl * yl - 2 * (e.A.X * p1.X + e.A.Y * p1.Y - e.A.Y * yl);

        x = (-b + sqrt(b * b - 4 * a * c)) / (2 * a);

        if(x < 0)
            cout << "WHAAAAAAAAAAAAAAT?!!!\n";

        x = x * e.n.X + e.A.X;

        if(p2.X <= x)
            break;
        else
        {
            xl = x;
            it++;
            e_it_p = e_it_n++;
        }
    }
    i = *it;
    p1 = points[i];

    e.a = na;
    e.b = *it;
    //e.A = mp((p1.X + p2.X) / 2, (p1.Y + p2.Y) / 2);
    e.A = mp(p2.X, ((p2.X - p1.X) * (p2.X - p1.X) + p1.Y * p1.Y - yl * yl) / (2 * (p1.Y - yl)));
    e.n = mp(p1.Y - p2.Y, p2.X - p1.X);
    rev_e = e;
    rev_e.n.X = -e.n.X;
    rev_e.n.Y = -e.n.Y;
    if(e.n.X > 0 || (e.n.X == 0 && e.n.Y < 0))
        swap(e, rev_e);

    nb = nd = -1;
    nc = *it;
    if(it != line.begin())
    {
        it--;
        nb = *it;
        e_p = edges[*e_it_p];
        it++;
    }
    it++;
    if(it != line.end())
    {
        nd = *it;
        e_n = edges[*e_it_n];
    }
    it--;

    if(nb != -1)
    {
        p1 = find_l(points[nb], points[nc], points[na]);
        p2 = find_c(points[nb], points[nc], points[na]);
        if(p1 != p_inf && check(e_p, p2) && check(e, p2))
            ord.push(mp(p1.Y, mp(mp(2, nb), mp(nc, na))));
    }

    if(nd != -1)
    {
        p1 = find_l(points[na], points[nc], points[nd]);
        p2 = find_c(points[na], points[nc], points[nd]);
        if(p1 != p_inf && check(rev_e, p2) && check(e_n, p2))
            ord.push(mp(p1.Y, mp(mp(2, na), mp(nc, nd))));
    }

    if(nb != -1 && nd != -1)
    {
        p1 = find_l(points[nb], points[nc], points[nd]);
        p2 = find_c(points[nb], points[nc], points[nd]);
        if(p1 != p_inf && check(e_p, p2) && check(e_n, p2))
            bad_ord.push(mp(p1.Y, mp(mp(2, nb), mp(nc, nd))));
    }

    line.insert(it, nc);
    line.insert(it, na);
    e_line.insert(e_it_n, edges.size());
    edges.push_back(e);
    e_line.insert(e_it_n, edges.size());
    edges.push_back(rev_e);

    /*for(auto it = line.begin(); it != line.end(); it++)
        cout << *it << " ";
    cout << endl;*/
}

void second_act()
{
    edge e1, e2, en, e_p, e_n;

    nb = act.X.Y;
    nc = act.Y.X;
    nd = act.Y.Y;

    p1 = points[nb];
    p2 = points[nc];
    p3 = points[nd];

    /*for(auto it = line.begin(); it != line.end(); it++)
        cout << *it << " ";
    cout << endl;*/

    auto it_p = line.begin();
    auto it = line.begin();
    it++;
    auto it_n = line.begin();
    it_n++;
    it_n++;
    auto e_it_p = e_line.begin();
    auto e_it1 = e_line.begin();
    auto e_it2 = e_line.begin();
    e_it2++;
    auto e_it_n = e_line.begin();
    e_it_n++;
    e_it_n++;

    while(true)
    {
        if(nb == *it_p && nc == *it && nd == *it_n)
            break;

        it_p++;
        it++;
        it_n++;
        e_it_p = e_it1++;
        e_it2++;
        e_it_n++;
    }

    edges[*e_it1].B = edges[*e_it2].B = en.A = find_c(p1, p2, p3);
    e1 = edges[*e_it1];
    e2 = edges[*e_it2];
    en.n = mp(e1.n.X + e2.n.X, e1.n.Y + e2.n.Y);
    if(e1.a == e2.a || e1.a == e2.b)
        swap(e1.a, e1.b);
    if(e2.b == e1.a || e2.b == e1.b)
        swap(e2.a, e2.b);
    en.a = e1.a;
    en.b = e2.b;

    na = ne = -1;
    if(it_p != line.begin())
    {
        it_p--;
        na = *it_p;
        e_p = edges[*e_it_p];
        it_p++;
    }
    it_n++;
    if(it != line.end())
    {
        ne = *it_n;
        e_n = edges[*e_it_n];
    }
    it_n--;

    if(na != -1)
    {
        p1 = find_l(points[na], points[nb], points[nd]);
        p2 = find_c(points[na], points[nb], points[nd]);
        if(p1 != p_inf && check(e_p, p2) && check(en, p2))
            ord.push(mp(p1.Y, mp(mp(2, na), mp(nb, nd))));
    }

    if(na != -1)
    {
        p1 = find_l(points[na], points[nb], points[nc]);
        p2 = find_c(points[na], points[nb], points[nc]);
        if(p1 != p_inf && check(e_p, p2) && check(e1, p2))
            bad_ord.push(mp(p1.Y, mp(mp(2, na), mp(nb, nc))));
    }

    if(ne != -1)
    {
        p1 = find_l(points[nb], points[nd], points[ne]);
        p2 = find_c(points[nb], points[nd], points[ne]);
        if(p1 != p_inf && check(en, p2) && check(e_n, p2))
            ord.push(mp(p1.Y, mp(mp(2, nb), mp(nd, ne))));
    }

    if(ne != -1)
    {
        p1 = find_l(points[nc], points[nd], points[ne]);
        p2 = find_c(points[nc], points[nd], points[ne]);
        if(p1 != p_inf && check(e2, p2) && check(e_n, p2))
            bad_ord.push(mp(p1.Y, mp(mp(2, nc), mp(nd, ne))));
    }

    /*for(i = 0; i < edges.size(); i++)
        cout << edges[i].A.X << " " << edges[i].A.Y << " "
             << edges[i].n.X << " " << edges[i].n.Y << " "
             << edges[i].B.X << " " << edges[i].B.Y << endl;

    for(auto it = e_line.begin(); it != e_line.end(); it++)
        cout << *it << " ";
    cout << endl;*/

    e_line.insert(e_it2, edges.size());
    edges.push_back(en);
    e_line.erase(e_it1);
    e_line.erase(e_it2);
    line.erase(it);

    /*for(i = 0; i < edges.size(); i++)
        cout << edges[i].A.X << " " << edges[i].A.Y << " "
             << edges[i].n.X << " " << edges[i].n.Y << " "
             << edges[i].B.X << " " << edges[i].B.Y << endl;

    for(auto it = e_line.begin(); it != e_line.end(); it++)
        cout << *it << " ";
    cout << endl;*/
}

int main()
{
    freopen("input.txt", "r", stdin);
    //freopen("output.out", "w", stdout);

    cin >> n;
    for(i = 0; i < n; i++)
    {
        cin >> x >> y;
        points.push_back(mp(-y, x));
    }

    sort(points.begin(), points.end());

    for(i = 0; i < n; i++)
    {
        swap(points[i].X, points[i].Y);
        points[i].Y = -points[i].Y;
        ord.push(mp(points[i].Y, mp(mp(1, i), mp(0, 0))));
        cout << points[i].X << " " << points[i].Y << endl;
    }

    while(!ord.empty())
    {
        if(!bad_ord.empty())
            if(ord.top() == bad_ord.top())
            {
                ord.pop();
                bad_ord.pop();
                continue;
            }

        yl = ord.top().X;
        act = ord.top().Y;
        ord.pop();

        if(act.X.X == 1)
            first_act();
        else
            second_act();
    }

    for(i = 0; i < edges.size(); i++)
        cout << edges[i].A.X << " " << edges[i].A.Y << " "
             << edges[i].n.X << " " << edges[i].n.Y << " "
             << edges[i].B.X << " " << edges[i].B.Y << endl;

    return 0;
}

















