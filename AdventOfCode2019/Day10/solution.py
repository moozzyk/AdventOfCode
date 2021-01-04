import math
import sys


def print_sky(sky):
    for l in sky:
        print("".join(l))
    print()


def atan2(beam_pos, star_pos):
    return math.atan2(star_pos[1] - beam_pos[1], star_pos[0] - beam_pos[0])


def distance(beam_pos, star_pos):
    return (star_pos[0] - beam_pos[0]) ** 2 + (star_pos[1] - beam_pos[1]) ** 2


def sort_stars(stars, beam_pos):
    return sorted(stars,
                  key=lambda s: (atan2(beam_pos, s), -distance(beam_pos, s)),
                  reverse=True)


def get_stars(sky):
    stars = []
    for row in range(len(sky)):
        for col in range(len(sky[row])):
            if sky[row][col] == "#":
                stars.append((row, col))
    return stars


def get_max_num_visible(sky):
    stars = get_stars(sky)

    last_atan = -100
    max_num_visible = -1
    max_num_visible_star = (-1, -1)
    for orig in stars:
        ordered = sort_stars(stars, orig)
        num_visible = 0
        for dest in ordered:
            if orig == dest:
                continue
            new_atan = atan2(orig, dest)
            if new_atan != last_atan:
                num_visible += 1
                last_atan = new_atan
        if num_visible > max_num_visible:
            max_num_visible = num_visible
            max_num_visible_star = orig
    return (max_num_visible, max_num_visible_star)


def shoot_stars(beam_pos, sky):
    stars = sort_stars(get_stars(sky), beam_pos)
    idx = 0
    shoot = True
    last_atan = -100
    num_shot = 0
    while True:
        star_pos = stars[idx]
        if sky[star_pos[0]][star_pos[1]] != "#":
            continue
        new_atan = atan2(beam_pos, stars[idx])
        if not shoot:
            shoot = new_atan != last_atan
        last_atan = new_atan

        if shoot:
            sky[star_pos[0]][star_pos[1]] = str(num_shot % 10)
            num_shot += 1
            if num_shot == 200:
                return star_pos
            shoot = False
            last_atan = new_atan
        idx = (idx + 1) % len(stars)


def problem1(sky):
    (max_num_visible, _) = get_max_num_visible(sky)
    print(max_num_visible)


def problem2(sky):
    (_, beam_pos) = get_max_num_visible(sky)
    (last_shot_row, last_shot_col) = shoot_stars(beam_pos, sky)
    print(last_shot_col * 100 + last_shot_row)


with open(sys.argv[1], "r") as f:
    sky = []
    for line in f.read().splitlines():
        sky.append(list(line))

    problem1(sky)
    problem2(sky)
