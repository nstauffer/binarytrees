#' Create an animation of radiating rosettes
#' @description Creates a looping animation of rosettes radiating out from the center and fading, one at a time.
#' @param n_layers Numeric. The number of layers of nodes. Must be greater than 3, should probably be less than 8.
#' @param output_path Character string. The filepath that the result should be written to. Defaults to the current working directory.
#' @param filename Character string. The filename for the output. Defaults to "animation.svg".
#' @param durations Named numeric vector. The duration in seconds that each step of the animation should take per rosette. Must include values named "draw", "hold", "fade", and "blank". Defaults to \code{c(draw = 2, hold = 0.5, fade = 0.5, blank = 0.25)}.
#' @param scale Numeric. The relative scaling to apply to the rosette spacing. Larger values will place the trees farther out from the center. Defaults to \code{1.4}.
#' @param tree_color Character string. The hex code for the color to draw the trees in. Defaults to \code{"#f87580"}.
#' @param background_Color Character string.  The hex code for the color to draw the background in. Defaults to \code{"#a5e8e3"}.
#' @param max_rosettes Numeric. The maximum number of rosettes to include in the animation. Defaults to \code{5}.
#' @param seed_number Numeric. The seed number for the randomized processes. Defaults to \code{69}.
#' @export
animate_rosettes <- function(n_layers,
                             output_path = getwd(),
                             filename = "animation.svg",
                             durations = c(draw = 2,
                                           hold = 0.5,
                                           fade = 0.5,
                                           blank = 0.25),
                             scale = 1.4,
                             tree_color = "#f87580",
                             background_color = "#a5e8e3",
                             max_rosettes = 5,
                             seed_number = 69) {
  trees <- generate_trees(n_layers = n_layers)

  trees <- arrange_rosettes(trees = trees,
                            scale = scale)

  # Keep only complete rosettes
  trees <- dplyr::filter(.data = trees,
                         rosette_id %in% unlist(sapply(X = unique(trees$rosette_id),
                                                       trees = trees,
                                                       FUN = function(X, trees){
                                                         if (all(1:6 %in% dplyr::filter(trees, rosette_id == X)$subunit_id)){
                                                           return(X)
                                                         }
                                                       })))

  set.seed(seed_number)
  trees <- dplyr::filter(.data = trees,
                         rosette_id %in% sample(x = unique(trees$rosette_id),
                                                size = min(max_rosettes, length(unique(trees$rosette_id)))))

  # Reset the rosette IDs
  trees <- dplyr::bind_rows(lapply(X = split(x = trees,
                                 f = trees$rosette_id),
                       current_rosette_ids = unique(trees$rosette_id),
                       FUN = function(X, current_rosette_ids){
                         X$rosette_id <- which(current_rosette_ids == X$rosette_id[1])
                         X
                       }))

  # Let's calculate some duration fractions. We'll use these to scale the increments
  # for the keyframes
  duration_fractions <- round(durations / sum(durations),
                              digits = 4)

  draw_percentage_interval <- 100 / (max(trees$end_layer_id) - 1)

  # Make the strings for the classes associated with the segments and their animations
  animation_string_list <- lapply(X = unique(trees$rosette_id),
                                  trees = trees,
                                  durations = durations,
                                  n_layers = max(trees$end_layer_id),
                                  FUN = function(X, trees, durations, n_layers){
                                    fraction_time <- 1 / length(unique(trees$rosette_id))

                                    draw_percentage_interval <- 100 / (n_layers - 1)

                                    draw_percentages <- round(seq(from = 0, length.out = n_layers, by = 1) * draw_percentage_interval * duration_fractions[["draw"]],
                                                              digits = 4)

                                    hold_percentages <- c(start = duration_fractions["draw"],
                                                          end = duration_fractions["draw"] + duration_fractions["hold"]) * 100

                                    fade_percentages <- c(start = duration_fractions["draw"] + duration_fractions["hold"],
                                                          end = duration_fractions["draw"] + duration_fractions["hold"] + duration_fractions["fade"]) * 100

                                    blank_percentages <- c(start = duration_fractions["draw"] + duration_fractions["hold"] + duration_fractions["fade"],
                                                           end = duration_fractions["draw"] + duration_fractions["hold"] + duration_fractions["fade"] + duration_fractions["blank"]) * 100

                                    # Build the keyframe components for each y ID that starts a segment
                                    keyframe_list <- lapply(X = seq(from = 1, to = n_layers - 1, by = 1),
                                                            draw_percentages = draw_percentages,
                                                            hold_percentages = hold_percentages,
                                                            fade_percentages = fade_percentages,
                                                            blank_percentages = blank_percentages,
                                                            fraction_time = fraction_time,
                                                            cycle = X,
                                                            FUN = function(X, draw_percentages, hold_percentages, fade_percentages, blank_percentages, fraction_time, cycle){
                                                              draw_keyframes <- unlist(lapply(X = 1:length(draw_percentages),
                                                                                              draw_percentages = draw_percentages,
                                                                                              cycle = cycle,
                                                                                              current_start_layer_id = X,
                                                                                              FUN = function(X, draw_percentages, cycle, current_start_layer_id){
                                                                                                if (X == current_start_layer_id + 1) {
                                                                                                  c(paste0((draw_percentages[X - 1] + 0.001) * fraction_time + (cycle - 1) * fraction_time * 100, "% { transform:scale(0); opacity:1;}"),
                                                                                                    paste0((draw_percentages[X]) * fraction_time + (cycle - 1) * fraction_time * 100, "% { transform:scale(1); opacity:1;}"))
                                                                                                } else if (X > current_start_layer_id + 1) {
                                                                                                  c(paste0((draw_percentages[X - 1] + 0.001) * fraction_time + (cycle - 1) * fraction_time * 100, "% { transform:scale(1); opacity:1;}"),
                                                                                                    paste0((draw_percentages[X]) * fraction_time + (cycle - 1) * fraction_time * 100, "% { transform:scale(1); opacity:1;}"))
                                                                                                } else if (X == 1) {
                                                                                                  paste0(min(0, (draw_percentages[X]) * fraction_time + (cycle - 1) * fraction_time * 100), "% { transform:scale(0); opacity:0;}")
                                                                                                } else {
                                                                                                  c(paste0((draw_percentages[X - 1] + 0.001) * fraction_time + (cycle - 1) * fraction_time * 100, "% { transform:scale(0); opacity:0;}"),
                                                                                                    paste0((draw_percentages[X]) * fraction_time + (cycle - 1) * fraction_time * 100, "% { transform:scale(0); opacity:0;}"))
                                                                                                }
                                                                                              }))
                                                              hold_keyframes <- c(paste0((hold_percentages[1] + 0.001) * fraction_time + (cycle - 1) * fraction_time * 100, "% { transform:scale(1); opacity:1;}"),
                                                                                  paste0(hold_percentages[2] * fraction_time + (cycle - 1) * fraction_time * 100, "% { transform:scale(1); opacity:1;}"))
                                                              fade_keyframes <- c(paste0((fade_percentages[1] + 0.001) * fraction_time + (cycle - 1) * fraction_time * 100, "% { transform:scale(1); opacity:1;}"),
                                                                                  paste0(fade_percentages[2] * fraction_time + (cycle - 1) * fraction_time * 100, "% { transform:scale(1); opacity:0;}"))
                                                              blank_keyframes <-c(paste0((blank_percentages[1] + 0.001) * fraction_time + (cycle - 1) * fraction_time * 100, "% { transform:scale(0); opacity:0;}"),
                                                                                  "100% { transform:scale(0); opacity:0;}")

                                                              c(draw_keyframes,
                                                                hold_keyframes,
                                                                fade_keyframes,
                                                                blank_keyframes)
                                                            })

                                    keyframe_strings <- sapply(X = 1:length(keyframe_list),
                                                               keyframe_list = keyframe_list,
                                                               rosette_id = X,
                                                               FUN = function(X, keyframe_list, rosette_id){
                                                                 paste0("@keyframes segment-start-y-id-", rosette_id, "-", X, "-anim {",
                                                                        paste(keyframe_list[[X]],
                                                                              collapse = " "),
                                                                        "}")
                                                               })

                                    segment_animation_class_strings <- sapply(X = seq(from = 1, to = n_layers - 1, by = 1),
                                                                              duration = sum(durations) * length(unique(trees$rosette_id)),
                                                                              rosette_id = X,
                                                                              FUN = function(X, duration, rosette_id){
                                                                                paste0(".segment-start-y-id-", rosette_id, "-", X, " {",
                                                                                       "animation: segment-start-y-id-", rosette_id, "-", X, "-anim; ",
                                                                                       "animation-duration: ", duration, "s; ",
                                                                                       "animation-timing-function: linear; ",
                                                                                       "animation-iteration-count: infinite;}")
                                                                              })

                                    list(keyframe_strings,
                                         segment_animation_class_strings)
                                  })

  header <- paste0("<svg xmlns = 'http://www.w3.org/2000/svg' width = '", 300, "' height = '", 300, "' viewBox = '-150 -150 300 300'>")

  # Make the paths
  # Each needs a distinct ID because each will have its own animation
  trees$path_string <- paste("<path",
                             "d =",
                             "'M", paste0(trees$start_x_coord * 100, ",", trees$start_y_coord * 100),
                             "L", paste0(trees$end_x_coord * 100, ",", trees$end_y_coord * 100), "'",
                             "id ='", paste(trees$rosette_id,
                                            trees$subunit_id,
                                            trees$start_layer_id,
                                            trees$start_within_layer_id,
                                            trees$end_layer_id,
                                            trees$end_within_layer_id,
                                            sep = "-"), "'",
                             "class =", paste0("'tree_segment ", "segment-start-y-id-", trees$rosette_id, "-", trees$start_layer_id, "'"),
                             "transform-origin =", paste0("'", trees$start_x_coord * 100, " ", trees$start_y_coord * 100, "'"),
                             # "vector-effect = 'non-scaling-stroke'",
                             "/>")

  # tree_segment_class_string <- paste0(".tree_segment {",
  #                                     "stroke-width: 5; ",
  #                                     "stroke: red; ",
  #                                     "vector-effect:non-scaling-stroke;}")
  tree_segment_class_string <- paste0(".tree_segment {",
                                      "stroke-width: 5; ",
                                      "stroke: ", tree_color, "; ",
                                      "stroke-linecap: round;}")

  output_vector <- c(header,
                     "<rect width = '300' height = '300' x = '-150' y = '-150' fill = '", background_color, "'/>",
                     trees$path_string,
                     "<style>",
                     tree_segment_class_string,
                     unlist(lapply(X = animation_string_list,
                                   FUN = function(X){X[[1]]})),
                     unlist(lapply(X = animation_string_list,
                                   FUN = function(X){X[[2]]})),
                     # segment_animation_class_strings,
                     # keyframe_strings,
                     "</style>",
                     "</svg>")

  writeLines(text = output_vector,
             con = paste0(output_path, "/", filename))
}

#' Create tiled rosettes
#' @description
#' Create a set of binary tree rosettes and tile them
#' @param n_layers Numeric. The number of layers of nodes in each tree. Must be greater than 3, should probably be less than 8.
#' @param n_rows Numeric. The number of rows to lay the rosettes in. Note that rows are not totally offset from each other in order to tessellate the rosettes. Defaults to \code{4}.
#' @param n_cols Numeric. The number of columns in each row to lay the rosettes in. Note that the actual number of rosettes across the results will be twice this. Defaults to \code{4}.
#' @param scale Numeric. The relative scaling to apply to the rosette spacing. Larger values will place the trees farther out from the center. Defaults to \code{1.4}.
#' @param seed_number Numeric. The seed number for the randomized processes. Defaults to \code{69}.
#' @export
tile_trees <- function(n_layers,
                       n_rows = 4,
                       n_cols = 4,
                       scale = 1.4,
                       seed_number = 69) {
  trees_df <- generate_trees(n_layers = n_layers)

  trees_arranged_df <- arrange_rosettes(trees = trees_df,
                                        scale = scale,
                                        seed_number = seed_number)

  output <- tile_rosettes(trees = trees_arranged_df,
                          n_cols = n_cols,
                          n_rows = n_rows,
                          seed_number = seed_number)

  output
}

#' Generate permutations of binary trees
#' @description
#' Create every permutation of binary trees (ignoring chirality) for a given number of layers. These are returned as coordinates defining line segments connecting nodes.
#' @param n_layers Numeric. The number of layers of nodes. Must be greater than 3, should probably be less than 8.
#' @export
generate_trees <- function(n_layers) {
  #### Find node coordinates ###################################################

  # These make up the node IDs where the row is along the y axis, top to bottom
  layer_ids <- unlist(sapply(X = 1:n_layers,
                             FUN = function(X){
                               rep(X,
                                   times = X)
                             }))

  # These are the column ID WITHIN THE ROW that the node belongs to (left to right)
  # We're doing it that way so we can easily make sure that we're only connecting
  # to nodes that are "adjacent"
  within_layer_ids <- unlist(sapply(X= 1:n_layers,
                                    FUN = function(X){
                                      seq(from = 1,
                                          to = X,
                                          by = 1)
                                    }))

  # The y increment is going to be [n_layers - 1] intervals along the long leg of the
  # right triangle made by bisecting this equilateral triangle
  y_increment <- sqrt(1^2 - (1 / 2)^2) / (n_layers - 1)

  # Across the x axis, we'll have n_layers - 1 columns at the base
  # My plan is to find how far the x ID is from the median and multiply this
  # increment by that then add the result to 0.5
  # For example, for the layer ID 3, there will be 3 nodes. The leftmost
  # within-layer ID is 1 the node count median is 2, making the difference -1.
  # Multiplying that by the column x increment then adding it to 0.5 should put
  # it right where it ought to be (I hope)
  x_increment <- 1 / (n_layers - 1)


  # Build the table of nodes, their x/layer IDs, and and coordinates
  nodes <- data.frame(layer_id = layer_ids,
                      within_layer_id = within_layer_ids)

  # So we can use the ID components and the increments to find Cartesian coords
  # This is a little ridiculous, but we're going to calculate our way down with
  # trig
  nodes_list <- apply(X = nodes,
                      n_layers = n_layers,
                      y_increment = y_increment,
                      x_increment = x_increment,
                      MARGIN = 1,
                      FUN = function(X, n_layers, y_increment, x_increment){
                        current_layer_id <- unlist(as.vector(X["layer_id"]))
                        current_within_layer_id <- unlist(as.vector(X["within_layer_id"]))

                        # This is finding the length short leg of a triangle
                        # made by bisecting the whole thing and cutting it off
                        # at the current row, then multiplying by two
                        row_width <- 2 * (current_layer_id - 1) * y_increment * tan(2 * pi / 6)

                        # So the layer_id should be the number of nodes we're
                        # dealing with to divide by spans between
                        row_increment <- row_width / (current_layer_id - 1)

                        # And now we determine the x coordinate as described
                        # where x_increment was calculated
                        x_coord <- 0.5 + (current_within_layer_id - median(unique(1:current_layer_id))) * x_increment

                        # Then the y coordinate is easier(?)
                        y_coord <- abs(current_layer_id - n_layers) * y_increment

                        output <- data.frame(within_layer_id = current_within_layer_id,
                                             layer_id = current_layer_id,
                                             x_coord = x_coord,
                                             y_coord = y_coord)

                        output
                      })

  nodes <- dplyr::bind_rows(nodes_list)

  #### Find unique node connection combinations ################################
  # First up are the line segments along the outer edges
  outer_nodes <- dplyr::filter(.data = nodes,
                               within_layer_id == 1 | within_layer_id == layer_id)

  outer_segment_list <- lapply(X = 1:nrow(outer_nodes),
                               outer_nodes = outer_nodes,
                               FUN = function(X, outer_nodes){
                                 current_layer_id <- unlist(as.vector(outer_nodes[X, "layer_id"]))
                                 current_within_layer_id <- unlist(as.vector(outer_nodes[X, "within_layer_id"]))
                                 message(paste(current_layer_id,
                                               current_within_layer_id,
                                               sep = ", "))

                                 start_node <- outer_nodes[X, ]

                                 names(start_node) <- paste0("start_", names(start_node))

                                 end_nodes <- dplyr::filter(.data = outer_nodes,
                                                            layer_id == current_layer_id + 1,
                                                            within_layer_id %in% c(current_within_layer_id, current_within_layer_id + 1))

                                 # Remove internal segments
                                 end_nodes <- dplyr::filter(.data = end_nodes,
                                                            within_layer_id %in% c(1, layer_id))

                                 names(end_nodes) <- paste0("end_", names(end_nodes))

                                 dplyr::bind_cols(start_node,
                                                  end_nodes)
                               })

  outer_segments <- dplyr::bind_rows(outer_segment_list)

  # And now the internal segments
  internal_segments_list <- lapply(X = which(nodes$layer_id %in% seq(from = 2, to = max(nodes$layer_id) - 1, by = 1)),
                                   nodes = nodes,
                                   FUN = function(X, nodes){
                                     current_layer_id <- unlist(as.vector(nodes[X, "layer_id"]))
                                     current_within_layer_id <- unlist(as.vector(nodes[X, "within_layer_id"]))
                                     message(paste(current_layer_id,
                                                   current_within_layer_id,
                                                   sep = ", "))

                                     start_node <- nodes[X, ]

                                     names(start_node) <- paste0("start_", names(start_node))

                                     end_nodes <- dplyr::filter(.data = nodes,
                                                                layer_id == current_layer_id + 1,
                                                                within_layer_id %in% c(current_within_layer_id, current_within_layer_id + 1))

                                     # Remove outer segments
                                     end_nodes <- dplyr::filter(.data = end_nodes,
                                                                !within_layer_id %in% c(1, layer_id))

                                     names(end_nodes) <- paste0("end_", names(end_nodes))

                                     dplyr::bind_cols(start_node,
                                                      end_nodes)
                                   })

  internal_segments <- dplyr::bind_rows(internal_segments_list)


  # We're gonna use for loops here to go layer ID by layer ID, from 1 to whatever and just
  # generate every connection set that's legal
  trees <- list(dplyr::filter(.data = outer_segments,
                              start_layer_id == 1))
  for (working_layer_id in 2:max(nodes$layer_id - 1)) {
    message(paste0("Working layer ID is ", working_layer_id))
    new_trees <- list()
    for (current_tree_index in 1:length(trees)) {
      message(paste0("Current tree index is ", current_tree_index, " of ", length(trees)))
      current_tree <- trees[[current_tree_index]]

      current_tree <- dplyr::mutate(.data = current_tree,
                                    id_string = paste(end_within_layer_id,
                                                      end_layer_id))

      available_internal_segments <- dplyr::filter(.data = internal_segments,
                                                   start_layer_id == working_layer_id,
                                                   start_within_layer_id %in% current_tree[current_tree$end_layer_id == working_layer_id, "end_within_layer_id"])

      available_internal_segments <- dplyr::mutate(.data = available_internal_segments,
                                                   id_string = paste(start_within_layer_id,
                                                                     start_layer_id))

      combos_matrix <- as.matrix(expand.grid(lapply(X = 1:nrow(available_internal_segments),
                                                    FUN = function(X){
                                                      c(TRUE, FALSE)
                                                    })))

      for (combo_index in 1:nrow(combos_matrix)) {
        message(paste0("Current combo index ", combo_index, " of ", nrow(combos_matrix)))
        selection_vector <- as.vector(combos_matrix[combo_index, ])

        proposed_new_segments <- available_internal_segments[selection_vector, ]

        valid_connection_number <- all(sapply(X = split(x = proposed_new_segments,
                                                        f = proposed_new_segments$end_within_layer_id),
                                              FUN = nrow) < 2)

        valid_connections_forward <- all(dplyr::filter(.data = current_tree,
                                                       end_layer_id == working_layer_id)$id_string %in% c(proposed_new_segments$id_string,
                                                                                                          paste(1, working_layer_id),
                                                                                                          paste(working_layer_id, working_layer_id)))

        if (valid_connection_number & valid_connections_forward) {
          new_tree <- dplyr::bind_rows(current_tree,
                                       dplyr::filter(outer_segments,
                                                     start_layer_id == working_layer_id),
                                       proposed_new_segments)

          new_trees[[paste(working_layer_id,
                           current_tree_index,
                           combo_index,
                           sep = "-")]] <- new_tree
        }
      }
    }
    trees <- new_trees
  }

  # Add in the ID var
  trees <- lapply(X = seq(from = 1,
                          by = 1,
                          length.out = length(trees)),
                  trees = trees,
                  FUN = function(X, trees){
                    current_tree <- trees[[X]]
                    current_tree$tree_id <- X
                    dplyr::select(current_tree,
                                  tree_id,
                                  dplyr::matches(match = "^start.*id$"),
                                  dplyr::matches(match = "^end.*id$"),
                                  dplyr::matches(match = "x_coord"),
                                  dplyr::matches(match = "y_coord"))
                  })

  # And return the output as a data frame
  dplyr::bind_rows(trees)
}

#' Arrange binary trees into hexagonal rosettes.
#' @description
#' Group trees into hexagons, rotating and translating them appropriately.
#' @param trees Data frame. Output from \code{generate_trees()}.
#' @param scale Numeric. The relative scaling to apply to the rosette spacing. Larger values will place the trees farther out from the center. Defaults to \code{1.4}.
#' @param seed_number Numeric. The seed number for the randomization of assignment of trees to rosettes. Defaults to \code{69}.
#' @export
arrange_rosettes <- function(trees,
                             scale = 1.4,
                             seed_number = 69) {
  # This'll make a collection of
  id_table <- expand.grid(subunit_id = 1:6,
                          rosette_id = seq(from = 1,
                                           by = 1,
                                           length.out = ceiling(length(unique(trees[["tree_id"]])) / 6)))

  rosette_subunit_centers <- vertices(vertex_count = 6,
                                      radius = abs(trees[["start_y_coord"]][1] - trees[["end_y_coord"]][1]) * (max(trees[["end_layer_id"]]) - 1) / 2 * 1.4,
                                      center_x = 0,
                                      center_y = 0,
                                      radian_offset = -3 * 2 * pi / (6 * 2),
                                      digits = 3)
  rosette_subunit_centers$id <- 1:6

  # Now we'll translate to the origin, rotate appropriately, and then translate
  # to the placement in the rosette
  set.seed(seed_number)
  sequence <- sample(seq(from = 1,
                         by = 1,
                         length.out = length(unique(trees[["tree_id"]]))),
                     size = length(unique(trees[["tree_id"]])))
  trees_arranged <- lapply(X = sequence,
                           sequence = sequence,
                           trees = trees,
                           tree_ids = unique(trees[["tree_id"]]),
                           id_table = id_table,
                           rosette_subunit_centers = rosette_subunit_centers,
                           FUN = function(X, sequence, trees, tree_ids, id_table, rosette_subunit_centers){
                             message(X)
                             current_tree <- trees[trees[["tree_id"]] == tree_ids[X], ]
                             current_tree$tree_id <- X
                             current_tree$subunit_id <- id_table[["subunit_id"]][which(sequence == X)]
                             current_tree$rosette_id <- id_table[["rosette_id"]][which(sequence == X)]

                             # So we can translate to center on (0, 0)
                             y_shift <- (max(current_tree$start_y_coord) - min(current_tree$end_y_coord)) / 2
                             x_shift <- (max(current_tree$end_x_coord) - min(current_tree$end_x_coord)) / 2

                             # Rotate the coordinates the correct increment
                             # around the origin via polar coordinates
                             rotated_coords <- dplyr::bind_rows(lapply(X = 1:nrow(current_tree),
                                                                       current_tree = current_tree,
                                                                       x_shift = x_shift,
                                                                       y_shift = y_shift,
                                                                       FUN = function(X, current_tree, x_shift, y_shift){
                                                                         # message(X)
                                                                         rotation_increment <- 2 * pi / 6
                                                                         data.frame(start_x_coord = round(rotate(x_coord = current_tree[X, "start_x_coord"] - x_shift,
                                                                                                                 y_coord = current_tree[X, "start_y_coord"] - y_shift,
                                                                                                                 angle_rad = rotation_increment * (current_tree[X, "subunit_id"] - 1))["x"],
                                                                                                          digits = 3),
                                                                                    start_y_coord = round(rotate(x_coord = current_tree[X, "start_x_coord"] - x_shift,
                                                                                                                 y_coord = current_tree[X, "start_y_coord"] - y_shift,
                                                                                                                 angle_rad = rotation_increment * (current_tree[X, "subunit_id"] - 1))["y"],
                                                                                                          digits = 3),
                                                                                    end_x_coord = round(rotate(x_coord = current_tree[X, "end_x_coord"] - x_shift,
                                                                                                               y_coord = current_tree[X, "end_y_coord"] - y_shift,
                                                                                                               angle_rad = rotation_increment * (current_tree[X, "subunit_id"] - 1))["x"],
                                                                                                        digits = 3),
                                                                                    end_y_coord = round(rotate(x_coord = current_tree[X, "end_x_coord"] - x_shift,
                                                                                                               y_coord = current_tree[X, "end_y_coord"] - y_shift,
                                                                                                               angle_rad = rotation_increment * (current_tree[X, "subunit_id"] - 1))["y"],
                                                                                                        digits = 3))
                                                                       }))

                             current_tree <- dplyr::bind_cols(dplyr::select(current_tree,
                                                                            -dplyr::matches("coord")),
                                                              rotated_coords)

                             # Then move it into position in the rosette
                             x_shift <- rosette_subunit_centers[rosette_subunit_centers$id == current_tree$subunit_id[1], "x"]
                             y_shift <- rosette_subunit_centers[rosette_subunit_centers$id == current_tree$subunit_id[1], "y"]

                             current_tree <- dplyr::mutate(.data = current_tree,
                                                           start_x_coord = start_x_coord + x_shift,
                                                           start_y_coord = start_y_coord + y_shift,
                                                           end_x_coord = end_x_coord + x_shift,
                                                           end_y_coord = end_y_coord + y_shift)
                             current_tree
                           })


  dplyr::select(dplyr::bind_rows(trees_arranged),
                tree_id, subunit_id, rosette_id,
                dplyr::ends_with(match = "layer_id"),
                start_x_coord = start_x_coord,
                end_x_coord = end_x_coord,
                start_y_coord = start_y_coord,
                end_y_coord = end_y_coord)
}
#' Arrange hexagonal rosettes of binary trees into tessellated layouts.
#' @description
#' Creates tessellated arrangement of the provided rosettes.
#' @param trees Data frame. The output from \code{arrange_rosettes()}.
#' @param n_rows Numeric. The number of rows to lay the rosettes in. Note that rows are not totally offset from each other in order to tessellate the rosettes. Defaults to \code{4}.
#' @param n_cols Numeric. The number of columns in each row to lay the rosettes in. Note that the actual number of rosettes across the results will be twice this. Defaults to \code{4}.
#' @param seed_number Numeric. The seed number for the randomization of rosette order. Defaults to \code{69}.
#' @export
tile_rosettes <- function(trees,
                          n_rows = 4,
                          n_cols = 4,
                          seed_number = 69) {

  # Figure out how far you'd need to shift a rosette to get it to line up nicely
  # with another for tesselation
  x_shift <- dplyr::filter(.data = trees,
                           rosette_id == 1,
                           subunit_id == 2,
                           end_layer_id == max(trees$end_layer_id),
                           end_within_layer_id == max(trees$end_layer_id))$end_x_coord -
    dplyr::filter(.data = trees,
                  rosette_id == 1,
                  subunit_id == 5,
                  end_layer_id == max(trees$end_layer_id),
                  end_within_layer_id == 1)$end_x_coord

  y_shift <- dplyr::filter(.data = trees,
                           rosette_id == 1,
                           subunit_id == 5,
                           end_layer_id == max(trees$end_layer_id),
                           end_within_layer_id == 1)$end_y_coord -
    dplyr::filter(.data = trees,
                  rosette_id == 1,
                  subunit_id == 2,
                  end_layer_id == max(trees$end_layer_id),
                  end_within_layer_id == max(trees$end_layer_id))$end_y_coord


  # Split into a list so we can work on each rosette in turn
  trees_arranged_list <- split(x = trees,
                               f = trees$rosette_id)

  # I only want complete rosettes
  trees_arranged_list <- trees_arranged_list[sapply(X = trees_arranged_list,
                                                    FUN = function(X){
                                                      all(1:6 %in% X$subunit_id)
                                                    })]


  # This makes sure that we get enough rosettes, even if that means repeats.
  # It also randomizes the order
  index_vector <- unlist(lapply(X = seq(from = 1, length.out = ceiling(n_cols * n_rows / length(trees_arranged_list)), by = seed_number),
                                available_indices = length(trees_arranged_list),
                                FUN = function(X, available_indices){
                                  set.seed(X)
                                  sample(x = 1:available_indices,
                                         size = available_indices)
                                }))[1:(n_rows * n_cols)]

  # Then we make a table of the number of shifts each rosette will make based
  # on the row and column
  frame_shifts <- dplyr::bind_rows(lapply(X = seq(from = 0, by = 1, length.out = n_rows),
                                          n_cols = n_cols,
                                          FUN = function(X, n_cols){
                                            if (X / 2 != round(X / 2)) {
                                              # It's odd
                                              data.frame(x_shift_increments = seq(from = 0,
                                                                                  by = 2,
                                                                                  length.out = n_cols),
                                                         y_shift_increments = X)
                                            } else {
                                              data.frame(x_shift_increments = seq(from = 1,
                                                                                  by = 2,
                                                                                  length.out = n_cols),
                                                         y_shift_increments = X)
                                            }
                                          }))


  trees_shifted <- lapply(X = seq(from = 1, length.out = n_rows * n_cols, by = 1),
                          index_vector = index_vector,
                          trees_arranged_list = trees_arranged_list,
                          x_shift = x_shift,
                          y_shift = y_shift,
                          frame_shifts = frame_shifts,
                          FUN = function(X, index_vector, trees_arranged_list, x_shift, y_shift, frame_shifts){
                            current_tree <- trees_arranged_list[[index_vector[X]]]

                            dplyr::mutate(.data = current_tree,
                                          start_x_coord = start_x_coord + x_shift * frame_shifts[X, "x_shift_increments"],
                                          end_x_coord = end_x_coord + x_shift * frame_shifts[X, "x_shift_increments"],
                                          start_y_coord = start_y_coord - y_shift * frame_shifts[X, "y_shift_increments"],
                                          end_y_coord = end_y_coord - y_shift * frame_shifts[X, "y_shift_increments"])
                          })

  dplyr::bind_rows(trees_shifted)
}

# Just to convert from polar to Cartesian systems and back
polar_to_cartesian <- function(radius,
                               angle_rad){
  c("x" = radius * cos(angle_rad),
    "y" = radius * sin(angle_rad))
}

cartesian_to_polar <- function(x_coord,
                               y_coord){
  c(radius = sqrt(x_coord^2 + y_coord^2),
    angle_rad = atan2(y_coord, x_coord))
}

# Rotate around the origin
rotate <- function(x_coord,
                   y_coord,
                   angle_rad){
  polar_coords <- cartesian_to_polar(x_coord = x_coord,
                                     y_coord = y_coord)

  polar_coords["angle_rad"] <- polar_coords["angle_rad"] + angle_rad %% (2 * pi)

  cartesian_coords_rotated <- polar_to_cartesian(radius = polar_coords["radius"],
                                                 angle_rad = polar_coords["angle_rad"])

  names(cartesian_coords_rotated) <- c("x", "y")

  cartesian_coords_rotated
}

#' Find vertices for a regular polygon
#' @description Given a circle defined by radius and center coordinates, find the vertices of a regular polygon inscribed in that circle.
#' @param vertex_count Numeric. The number of vertices the polygon has.
#' @param radius Numeric. The radius of the circle that the vertices fall along.
#' @param center_x Numeric. The x component of the coordinates for the circle's center.
#' @param center_y Numeric. The y component of the coordinates for the circle's center.
#' @param radian_offset Numeric. The rotational offset for the vertices in radians. If \code{0} then one vertex will be at (cos(0), sin(0)). Defaults to \code{0}.
#' @param digits Numeric. The number of decimal places to round the coordinates to. Defaults to \code{2}.
#' @export
#' @return A data frame with the variables \code{x}, \code{y}, \code{angle}, \code{radius}, \code{cx}, and \code{cy}.
vertices <- function(vertex_count,
                     radius,
                     center_x,
                     center_y,
                     radian_offset = 0,
                     digits = 2) {
  # First off, what's the spacing between the vertices in radians?
  radian_increment <- 2 / vertex_count

  # Okay! Back to high school trigonometry:
  # The x coordinate is equal to the cosine of the angle and the y is the sine.
  # We're solving for all the vertices at once, so we'll make an ordinal vector with
  # a length equal to the number of vertices but starting at 0 instead of 1.
  # Each of those gets multiplied by the radian increment and pi to give us the
  # angle for that vertex.
  # Then we add the radian offset (the rotation of the polygon around the center)
  # to find the *actual* angle we want to use.
  # Finally, we find the cosine and sine of those angles and multiply them by
  # the radius because otherwise we've got coordinates for points on a unit circle.
  angles <- 0:(vertex_count - 1) * radian_increment * pi + radian_offset

  x_vertices <- round(radius * cos(angles),
                      digits = digits)
  y_vertices <- round(radius * sin(angles),
                      digits = digits)

  # Our coordinates are currently for a circle centered at (0, 0), so we adjust
  # the coordinates using the center of the circle we actually want
  x_vertices_adjusted <- x_vertices + center_x
  y_vertices_adjusted <- y_vertices + center_y

  data.frame(x = x_vertices_adjusted,
             y = y_vertices_adjusted,
             angle = angles,
             radius = radius,
             cx = center_x,
             cy = center_y)
}

