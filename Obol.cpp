/* GCC */
#include <SFML/Graphics.hpp>

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

const double eps = 1e-9;
const double inf_ = 1e9;
const pair <double, double> p_inf(-inf_, -inf_);

const int ch = 30;
const int h1 = 0;
const int h2 = 599;
const int w1 = 0;
const int w2 = 799;

int sz = 0;
sf::VertexArray lines(sf::Lines, 0);
sf::RenderWindow window(sf::VideoMode(w2 + 1, h2 + 1), "Voronoi diagram");

struct edge
{
    pair <double, double> A = p_inf;
    pair <double, double> B = p_inf;
    pair <double, double> n = p_inf;

    int a = -1;
    int b = -1;

    pair <double, double> inf_edge()
    {
        double t = 1e6;
        return mp(n.X * t + A.X, n.Y * t + A.Y);
    }
};

vector <edge> edges;
vector < pair <double, double> > points;
double x, y, xl, yl, old_yl;
int n, i, j;
priority_queue < pair <double, pair < pair <int, int>, pair <int, int> > > > ord, bad_ord;
pair < pair <int, int>, pair <int, int> > act;
list <int> line;
list <int> e_line;

void swap_p()
{
    for(int i = 0; i < n; i++)
    {
        points[i].Y = -points[i].Y;
        swap(points[i].X, points[i].Y);
    }
}

void time_skip()
{
    int time = clock();
    int time_change = 1500;

    while(clock() - time < time_change)
    {
        sf::Event event;
        while(window.pollEvent(event))
        {
            if (event.type == sf::Event::Closed)
            {
                window.close();
                return;
            }
        }
    }
}

pair <double, double> find_c(pair <double, double> p1, pair <double, double> p2, pair <double, double> p3)
{
    double a, b, c, d, e, f, g;

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

bool is_equal(double a, double b)
{
    return abs(a - b) <= eps;
}

double find_l_x(const edge &e, double yl)
{
    double a, b, c, x;
    pair <double, double> p = (points[e.a].Y > points[e.b].Y ? points[e.a] : points[e.b]);
    a = e.n.X * e.n.X;
    b = 2 * (e.n.X * e.A.X - e.n.X * p.X - e.n.Y * p.Y + e.n.Y * yl);
    c = e.A.X * e.A.X + p.X * p.X + p.Y * p.Y - yl * yl - 2 * (e.A.X * p.X + e.A.Y * p.Y - e.A.Y * yl);

    if(is_equal(c, 0))
        c = 0;

    if(a != 0)
        x = (-b + sqrt(b * b - 4 * a * c)) / (2 * a);
    else if(b != 0)
        x = -c/b;
    else
        x = 0;

    if(x < 0)
        cout << "WHAAAAAAAAAAAAAAT?!!!\n";

    x = x * e.n.X + e.A.X;

    return x;
}

double find_l_y(double x, pair <double, double> p, double yl)
{
    return ((x - p.X) * (x - p.X) + p.Y * p.Y - yl * yl) / (2 * (p.Y - yl));
}

pair <double, double> find_l_p(const edge &e, double yl)
{
    pair <double, double> p;

    p.X = find_l_x(e, yl);
    p.Y = find_l_y(p.X, points[e.a], yl);

    return p;
}

bool check(const edge &e, pair <double, double> p)
{
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
    else if(is_equal(t1, t2))
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
    int na, nb, nc, nd;
    edge e, rev_e, e_p, e_n;
    pair <double, double> p1, p2;

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

        x = find_l_x(e, yl);

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
    //e.A = mp(p2.X, ((p2.X - p1.X) * (p2.X - p1.X) + p1.Y * p1.Y - yl * yl) / (2 * (p1.Y - yl)));
    e.A = mp(p2.X, find_l_y(p2.X, p1, yl));
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
}

void second_act()
{
    int na, nb, nc, nd, ne;
    edge e1, e2, en, e_p, e_n;
    pair <double, double> p1, p2, p3;

    nb = act.X.Y;
    nc = act.Y.X;
    nd = act.Y.Y;

    p1 = points[nb];
    p2 = points[nc];
    p3 = points[nd];

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

    e_line.insert(e_it2, edges.size());
    edges.push_back(en);
    e_line.erase(e_it1);
    e_line.erase(e_it2);
    line.erase(it);
}

double change_x(double x)
{
    return x * ch + w2/2;
}

double change_y(double y)
{
    return y * ch + h2/2;
}

pair <double, double> change_p(pair <double, double> p)
{
    return mp(change_x(p.X), change_y(p.Y));
}

double rev_y(double y)
{
    return h2 - y;
}

void draw_Voronoi_points(int type)
{
    sf::VertexArray quads(sf::Quads, 4 * n);

    int r = 5;
    int cur_x, cur_y;

    for(int i = 0; i < n; i++)
    {
        cur_x = change_x(points[i].X);
        cur_y = rev_y(change_y(points[i].Y));

        quads[4 * i + 0].position = sf::Vector2f(cur_x + r, cur_y + r);
        quads[4 * i + 1].position = sf::Vector2f(cur_x + r, cur_y - r);
        quads[4 * i + 2].position = sf::Vector2f(cur_x - r, cur_y - r);
        quads[4 * i + 3].position = sf::Vector2f(cur_x - r, cur_y + r);

        if(type == 0 && (i == act.X.Y || i == act.Y.X || i == act.Y.Y))
        {
            quads[4 * i + 0].color = sf::Color::Red;
            quads[4 * i + 1].color = sf::Color::Red;
            quads[4 * i + 2].color = sf::Color::Red;
            quads[4 * i + 3].color = sf::Color::Red;

        }
        else
        {
            quads[4 * i + 0].color = sf::Color::Black;
            quads[4 * i + 1].color = sf::Color::Black;
            quads[4 * i + 2].color = sf::Color::Black;
            quads[4 * i + 3].color = sf::Color::Black;
        }
    }

    window.draw(quads);
}

void draw_Voronoi_coastline(int type)
{
    if(type == 0)
    {
        if(old_yl != yl)
        {
            sz = 0;
            lines.clear();
        }
    }

    sf::VertexArray c_line(sf::Points, w2 + 1);

    int left_x = 0;
    int right_x;
    int cur_x, cur_y;

    auto it = line.begin();
    auto e_it = e_line.begin();

    right_x = -1;
    while(e_it != e_line.end() && right_x <= 0)
    {
        //if(*it != bad_p)
        if(points[*it].Y != yl)
            right_x = min(change_x(find_l_x(edges[*e_it], yl)), double(w2));
        e_it++;
        it++;
    }
    if(right_x <= 0)
        right_x = w2;
    else
        it--;

    while(right_x < w2)
    {
        //point_num = *it;

        for(cur_x = left_x; cur_x < right_x; cur_x++)
        {
            cur_y = find_l_y(cur_x, change_p(points[*it]), change_y(yl));
            cur_y = rev_y(cur_y);
            c_line[cur_x].position = sf::Vector2f(cur_x, cur_y);
            c_line[cur_x].color = sf::Color::Green;
        }

        it++;
        //cur_x = points[*it].X;
        //cur_y = points[*it].Y;
        //if(*it == bad_p)
        if(points[*it].Y == yl)
        {
            //sz = lines.size();
            lines.resize(sz + 2);

            cur_x = change_x(points[*it].X);
            cur_y = rev_y(change_y(yl));
            lines[sz + 0].position = sf::Vector2f(cur_x, cur_y);
            it++;
            cur_y = rev_y(find_l_y(cur_x, change_p(points[*it]), change_y(yl)));
            lines[sz + 1].position = sf::Vector2f(cur_x, cur_y);
            lines[sz + 0].color = sf::Color::Green;
            lines[sz + 1].color = sf::Color::Green;
            sz += 2;

            e_it++;
        }

        left_x = right_x;
        if(e_it == e_line.end())
            right_x = w2;
        else
        {
            //point_num = *it;
            right_x = min(change_x(find_l_x(edges[*e_it], yl)), double(w2));
            e_it++;
        }
    }

    if(points[*it].Y != yl)
    {
        for(cur_x = left_x; cur_x <= right_x; cur_x++)
        {
            cur_y = find_l_y(cur_x, change_p(points[*it]), change_y(yl));
            cur_y = rev_y(cur_y);
            c_line[cur_x].position = sf::Vector2f(cur_x, cur_y);
            c_line[cur_x].color = sf::Color::Green;
        }

        window.draw(c_line);
    }
    else
    {
        //sz = lines.size();
        lines.resize(sz + 2);
        lines[sz + 0].position = sf::Vector2f(change_x(points[*it].X), rev_y(change_y(points[*it].Y)));
        lines[sz + 1].position = sf::Vector2f(change_x(points[*it].X), rev_y(change_y(inf_)));
        lines[sz + 0].color = sf::Color::Green;
        lines[sz + 1].color = sf::Color::Green;
        sz += 2;
    }

    window.draw(lines);
}

void draw_Voronoi_edges(int type)
{
    sf::VertexArray v_edges(sf::Lines, edges.size() * 2);

    for(i = 0; i < edges.size(); i++)
    {
        pair <double, double> A = edges[i].A;
        pair <double, double> B;

        if(edges[i].B == p_inf)
        {
            if(type == 0)
                B = find_l_p(edges[i], yl);
            else
                B = edges[i].inf_edge();
        }
        else
            B = edges[i].B;

        v_edges[2 * i + 0].position = sf::Vector2f(change_x(A.X), rev_y(change_y(A.Y)));
        v_edges[2 * i + 1].position = sf::Vector2f(change_x(B.X), rev_y(change_y(B.Y)));

        v_edges[2 * i + 0].color = sf::Color::Black;
        v_edges[2 * i + 1].color = sf::Color::Black;
    }

    window.draw(v_edges);
}

void draw_Voronoi_obol()
{
    bool up_is_line, low_is_line;
    vector <bool> flag(n, false);
    vector <int> obol_points;

    for(auto e_it = e_line.begin(); e_it != e_line.end(); e_it++)
        if(edges[*e_it].B == p_inf)
            flag[edges[*e_it].a] = flag[edges[*e_it].b] = true;

    for(int i = 0; i < n; i++)
        if(flag[i] == true)
            obol_points.push_back(i);

    if(obol_points.size() > 0)
    {
        double a, b, c;
        a = points[obol_points[0]].Y - points[obol_points.back()].Y;
        b = points[obol_points.back()].X - points[obol_points[0]].X;
        c = - (a * points[obol_points[0]].X + b * points[obol_points[0]].Y);

        int i;
        up_is_line = low_is_line = true;
        for(i = 0; i < obol_points.size(); i++)
            if(a * points[obol_points[i]].X + b * points[obol_points[i]].Y + c > 0)
                up_is_line = false;

        for(i -= 2; i >= 0; i--)
            if(a * points[obol_points[i]].X + b * points[obol_points[i]].Y + c < 0)
                low_is_line = false;

        sf::VertexArray obol(sf::LineStrip, obol_points.size() + 1);

        int j = 0;
        obol[j].position = sf::Vector2f(change_x(points[obol_points[0]].X), rev_y(change_y(points[obol_points[0]].Y)));
        obol[j++].color = sf::Color::Red;

        for(i = 1; i + 1 < obol_points.size(); i++)
            if(a * points[obol_points[i]].X + b * points[obol_points[i]].Y + c >= 0)
            {
                if(a * points[obol_points[i]].X + b * points[obol_points[i]].Y + c == 0 && low_is_line == true)
                    continue;

                obol[j].position = sf::Vector2f(change_x(points[obol_points[i]].X), rev_y(change_y(points[obol_points[i]].Y)));
                obol[j++].color = sf::Color::Red;
            }

        obol[j].position = sf::Vector2f(change_x(points[obol_points[i]].X), rev_y(change_y(points[obol_points[i]].Y)));
        obol[j++].color = sf::Color::Red;

        for(i--; i > 0; i--)
            if(a * points[obol_points[i]].X + b * points[obol_points[i]].Y + c <= 0)
            {
                if(a * points[obol_points[i]].X + b * points[obol_points[i]].Y + c == 0 && low_is_line == false && up_is_line == true)
                    continue;

                obol[j].position = sf::Vector2f(change_x(points[obol_points[i]].X), rev_y(change_y(points[obol_points[i]].Y)));
                obol[j++].color = sf::Color::Red;
            }

        obol[j].position = sf::Vector2f(change_x(points[obol_points[0]].X), rev_y(change_y(points[obol_points[0]].Y)));
        obol[j++].color = sf::Color::Red;

        window.draw(obol);
    }
}

void draw_Voronoi_triangle()
{
    sf::VertexArray triangle(sf::Lines, edges.size() * 2);

    int i = 0;
    for(auto e_it = edges.begin(); e_it != edges.end(); e_it++)
        if(e_it->A != e_it->B)
        {
            pair <double, double> p1, p2;

            p1 = points[e_it->a];
            p2 = points[e_it->b];

            triangle[2 * i + 0].position = sf::Vector2f(change_x(p1.X), rev_y(change_y(p1.Y)));
            triangle[2 * i + 1].position = sf::Vector2f(change_x(p2.X), rev_y(change_y(p2.Y)));
            triangle[2 * i + 0].color = sf::Color::Red;
            triangle[2 * i + 1].color = sf::Color::Red;
            i++;
        }

    window.draw(triangle);
}

void draw_Voronoi(int type)
{
    window.clear(sf::Color::White);

    sf::VertexArray l(sf::Lines, 2);
    l[0].position = sf::Vector2f(w1, rev_y(change_y(yl)));
    l[1].position = sf::Vector2f(w2, rev_y(change_y(yl)));
    l[0].color = sf::Color::Blue;
    l[1].color = sf::Color::Blue;

    draw_Voronoi_edges(type);
    if(type == 0)
    {
        //int bad_p = (act.X.X == 1 ? act.X.Y : -1);
        draw_Voronoi_coastline(type);
        window.draw(l);
    }
    else
        draw_Voronoi_obol();
    draw_Voronoi_points(type);

    window.display();
}

void Voronoi(int type)
{
    swap_p();

    sort(points.begin(), points.end());

    for(i = 0; i < n; i++)
    {
        swap(points[i].X, points[i].Y);
        points[i].Y = -points[i].Y;
        ord.push(mp(points[i].Y, mp(mp(1, i), mp(-1, -1))));
    }

    yl = inf_;
    while(!ord.empty())
    {
        if(!bad_ord.empty())
            if(ord.top() == bad_ord.top())
            {
                ord.pop();
                bad_ord.pop();
                continue;
            }

        old_yl = yl;
        yl = ord.top().X;
        act = ord.top().Y;
        ord.pop();

        if(act.X.X == 1)
            first_act();
        else
            second_act();

        if(type == 1)
        {
            draw_Voronoi(0);
            time_skip();
        }
    }

    if(type == 1)
        draw_Voronoi(1);
}

void Delone()
{
    Voronoi(0);

    window.clear(sf::Color::White);
    //draw_Voronoi_edges(1);
    draw_Voronoi_triangle();
    draw_Voronoi_points(1);

    window.display();
}

bool left_move(pair <double, double> p1, pair <double, double> p2, pair <double, double> p3)
{
    return (p1.X * (p2.Y - p3.Y) + p2.X * (p3.Y - p1.Y) + p3.X * (p1.Y - p2.Y)) > 0;
}

bool right_move(pair <double, double> p1, pair <double, double> p2, pair <double, double> p3)
{
    return (p1.X * (p2.Y - p3.Y) + p2.X * (p3.Y - p1.Y) + p3.X * (p1.Y - p2.Y)) < 0;
}

void draw_Keyla(const vector < pair <double, double> > &left_part, const vector < pair <double, double> > &right_part, const sf::VertexArray &quads)
{
    int i;
    int sz = 2 * (max(left_part.size(), 1u) + max(right_part.size(), 1u) - 2);

    if(sz == 0)
        return;

    sf::VertexArray lines(sf::Lines, sz);

    pair <double, double> A, B;

    for(i = 0; i + 1 < left_part.size(); i++)
    {
        A = left_part[i];
        B = left_part[i + 1];

        lines[2 * i + 0].position = sf::Vector2f(change_x(A.X) - 1, rev_y(change_y(A.Y) - 1));
        lines[2 * i + 1].position = sf::Vector2f(change_x(B.X) - 1, rev_y(change_y(B.Y) - 1));

        lines[2 * i + 0].color = sf::Color::Red;
        lines[2 * i + 1].color = sf::Color::Red;
    }

    sz = i;
    for(i = 0; i + 1 < right_part.size(); i++)
    {
        A = right_part[i];
        B = right_part[i + 1];

        lines[2 * (i + sz) + 0].position = sf::Vector2f(change_x(A.X) + 1, rev_y(change_y(A.Y) + 1));
        lines[2 * (i + sz) + 1].position = sf::Vector2f(change_x(B.X) + 1, rev_y(change_y(B.Y) + 1));

        lines[2 * (i + sz) + 0].color = sf::Color::Blue;
        lines[2 * (i + sz) + 1].color = sf::Color::Blue;
    }

    window.clear(sf::Color::White);
    window.draw(lines);
    window.draw(quads);
    window.display();
    time_skip();
}

void Keyla()
{
    sf::VertexArray quads(sf::Quads, 4 * n);
    const int r = 5;

    const int max_h = 100010;

    int cur_x, cur_y;
    int x, y;
    int yl[max_h], yr[max_h];
    vector < pair <double, double> > left_part, right_part;
    pair <double, double> p1, p2, p3;
    //vector < pair <int, int> > points;

    fill(yl, yl + max_h, inf_);
    fill(yr, yr + max_h, 0);

    for(i = 0; i < n; i++)
    {
        yl[int(points[i].Y)] = min(yl[int(points[i].Y)], int(points[i].X));
        yr[int(points[i].Y)] = max(yr[int(points[i].Y)], int(points[i].X));

        //cout << i << " " << points[i].X << " " << points[i].Y << " " << yl[int(points[i].Y)] << " " << yr[int(points[i].Y)] << endl;
    }

    for(i = 0; i < n; i++)
    {
        cur_x = change_x(points[i].X);
        cur_y = rev_y(change_y(points[i].Y));

        quads[4 * i + 0].position = sf::Vector2f(cur_x + r, cur_y + r);
        quads[4 * i + 1].position = sf::Vector2f(cur_x + r, cur_y - r);
        quads[4 * i + 2].position = sf::Vector2f(cur_x - r, cur_y - r);
        quads[4 * i + 3].position = sf::Vector2f(cur_x - r, cur_y + r);

        if(yl[int(points[i].Y)] == points[i].X && yr[int(points[i].Y)] == points[i].X)
        {
            quads[4 * i + 0].color = sf::Color::Green;
            quads[4 * i + 1].color = sf::Color::Green;
            quads[4 * i + 2].color = sf::Color::Green;
            quads[4 * i + 3].color = sf::Color::Green;

        }
        else if(yl[int(points[i].Y)] == points[i].X)
        {
            quads[4 * i + 0].color = sf::Color::Red;
            quads[4 * i + 1].color = sf::Color::Red;
            quads[4 * i + 2].color = sf::Color::Red;
            quads[4 * i + 3].color = sf::Color::Red;

        }
        else if(yr[int(points[i].Y)] == points[i].X)
        {
            quads[4 * i + 0].color = sf::Color::Blue;
            quads[4 * i + 1].color = sf::Color::Blue;
            quads[4 * i + 2].color = sf::Color::Blue;
            quads[4 * i + 3].color = sf::Color::Blue;

        }
        else
        {
            quads[4 * i + 0].color = sf::Color::Black;
            quads[4 * i + 1].color = sf::Color::Black;
            quads[4 * i + 2].color = sf::Color::Black;
            quads[4 * i + 3].color = sf::Color::Black;
        }
    }

    window.clear(sf::Color::White);
    window.draw(quads);
    window.display();
    time_skip();

    p3 = p_inf;
    for(y = 0; y < max_h; y++)
        if(yr[y] != 0)
        {
            if(p3 == p_inf && yl[y] != yr[y])
                right_part.push_back(mp(yl[y], y));
            p3 = mp(yl[y], y);

            while(left_part.size() > 1)
            {
                p1 = left_part[left_part.size() - 2];
                p2 = left_part[left_part.size() - 1];

                if(left_move(p1, p2, p3) == false)
                    break;

                left_part.push_back(p3);
                draw_Keyla(left_part, right_part, quads);
                left_part.pop_back();
                left_part.pop_back();
            }

            left_part.push_back(p3);
            draw_Keyla(left_part, right_part, quads);

            p3 = mp(yr[y], y);

            while(right_part.size() > 1)
            {
                p1 = right_part[right_part.size() - 2];
                p2 = right_part[right_part.size() - 1];

                if(right_move(p1, p2, p3) == false)
                    break;

                right_part.push_back(p3);
                draw_Keyla(left_part, right_part, quads);
                right_part.pop_back();
                right_part.pop_back();
            }

            right_part.push_back(p3);
            draw_Keyla(left_part, right_part, quads);
        }

    p2 = left_part.back();
    if(p3 != left_part.back())
    {
        left_part.push_back(p3);
        draw_Keyla(left_part, right_part, quads);
    }
}

void Jarvis()
{
    pair <double, double> p1, p2, p3;
    sf::VertexArray obol_lines(sf::Lines, 0);
    sf::VertexArray int_lines(sf::Lines, 4);
    int obol_lines_size = 0;

    int i, j, k;
    for(i = 0; i + 2 < n; i++)
        for(j = i + 1; j + 1 < n; j++)
        {


            bool flag = false;
            for(k = j + 1; k < n; k++)
        }
}

void Greham()
{

}

void Obol()
{

}

int main()
{
    freopen("input.txt", "r", stdin);
    //freopen("input.in", "r", stdin);
    //freopen("output.out", "w", stdout);

    cin >> n;
    for(i = 0; i < n; i++)
    {
        cin >> x >> y;
        points.push_back(mp(x, y));
    }

    //Voronoi(1);
    //Delone();
    Keyla();

    while (window.isOpen())
    {
        sf::Event event;
        while (window.pollEvent(event))
        {
            if (event.type == sf::Event::Closed)
                window.close();
        }
    }

    return 0;
}

















