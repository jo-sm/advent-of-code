#lang racket

(regexp-match* #px".................#" (list-ref (file->lines "test") 2))
(regexp-match* #px"#....##....##....###" (list-ref (file->lines "test") 3))
(regexp-match* #px"#..#..#..#..#..#" (list-ref (file->lines "test") 4))
