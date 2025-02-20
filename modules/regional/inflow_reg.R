# Function to create curved lines with gradient colors
create_curved_gradient <- function(x1, y1, x2, y2, curvature = 0.2, n = 100) {
  # Calculate control points for the Bezier curve
  mid_x <- (x1 + x2) / 2
  mid_y <- (y1 + y2) / 2
  control_x <- mid_x + curvature * (y2 - y1)
  control_y <- mid_y - curvature * (x2 - x1)
  
  # Generate points along the Bezier curve
  t <- seq(0, 1, length.out = n)
  x <- (1 - t)^2 * x1 + 2 * (1 - t) * t * control_x + t^2 * x2
  y <- (1 - t)^2 * y1 + 2 * (1 - t) * t * control_y + t^2 * y2
  
  tibble(
    x = x,
    y = y,
    gradient = t  # Gradient from 0 (origin) to 1 (destination)
  )
}
