mod type_utils;
use crate::type_utils::type_utils::Color;

fn main() {
    let image_width = 256;
    let image_height = 256;
    print!("P3\n {0} {1} \n255\n", image_width, image_height);
    let pb = indicatif::ProgressBar::new(image_height - 1);
    pb.println("hello");
    for j in 0..image_height {
        pb.inc(1);
        for i in 0..image_width {
            let r = (i as f64) / (image_width - 1) as f64;
            let g = (j as f64) / (image_height - 1) as f64; 
            let b = 0.0;
            let color = Color::newv(255.999 * r, 255.999 * g, 255.999 * b);
            color.write_color();
        }
    }
}
