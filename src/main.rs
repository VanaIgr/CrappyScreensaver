extern crate glfw;
extern crate gl;
extern crate image;
extern crate rand;

#[macro_use]
extern crate static_assertions;

macro_rules! checkError {
    () => (unsafe {
        let l = line!();
        let mut err: u32;
        while ((err = gl::GetError()), false).1 | (err != gl::NO_ERROR) {
            let error: &str = match err {
                gl::INVALID_ENUM =>                   "INVALID_ENUM",
                gl::INVALID_VALUE =>                  "INVALID_VALUE",
                gl::INVALID_OPERATION =>              "INVALID_OPERATION",
                gl::STACK_OVERFLOW =>                 "STACK_OVERFLOW",
                gl::STACK_UNDERFLOW =>                "STACK_UNDERFLOW",
                gl::OUT_OF_MEMORY =>                  "OUT_OF_MEMORY",
                gl::INVALID_FRAMEBUFFER_OPERATION =>  "INVALID_FRAMEBUFFER_OPERATION",
                _ =>  "UNKNOWN ERROR"
            };
            println!("line {}, Error: {}", l, error);
        }
    })
}

macro_rules! cstr {
    ($str:expr) => (concat!($str, "\0").as_ptr() as *const i8)
}

fn lerpf(a: f32, b: f32, f: f32) -> f32 {
    return a + (b - a) * f;
}

fn normalize(x: f32, y: f32) -> (f32, f32) {
    let invLen = (1.0 / (x*x + y*y)).sqrt();
    return (x * invLen, y * invLen);
}

struct Screensaver {
    velX: f32, velY: f32,
    x: f32, y: f32,
    blockWidth: f32, blockHeight: f32,
    cornerAssist: bool,
    cornerCount: i32
}

const startVel: f32 = 1.0 / 450.0;
impl Screensaver {

    fn new(blockWidth: f32, blockHeight: f32, cornerAssist: bool) -> Screensaver {
        Screensaver {
            velX: startVel, velY: startVel,
            x: blockWidth * 0.5, y: blockHeight * 0.5,
            blockWidth: blockWidth, blockHeight: blockHeight,
            cornerAssist: cornerAssist,
            cornerCount: 0
        }
    }

    fn update(&mut self, widthFac: f32) {
        let mut velX = self.velX;
        let mut velY = self.velY;
        let mut x = self.x;
        let mut y = self.y;

        let offsetX = self.blockWidth * 0.5;
        let offsetY = self.blockHeight * 0.5;

        macro_rules! randFac {
            () => (0.3 +  rand::random::<f32>() * 1.1);
        }

        macro_rules! newSpeed {
            ($value:expr) => (lerpf($value.abs(), startVel * randFac!(), 0.3));
        }

        x += velX;
        y += velY;

        let mut corner = false;

        if velX > 0.0 && x + offsetX - widthFac >= 0.0 {
            x -= 2.0 * (x + offsetX - widthFac);
            velX = -newSpeed!(velX);
            corner = true;
        }
        else if velX <= 0.0 && x - offsetX < 0.0 {
            x -= 2.0 * (x - offsetX);
            velX = newSpeed!(velX);
            corner = true;
        }

        if velY > 0.0 && y + offsetY - 1.0 >= 0.0 {
            y -= 2.0 * (y + offsetY - 1.0);
            velY = -newSpeed!(velY);
            corner = true;
        }
        else if velY <= 0.0 && y - offsetY < 0.0 {
            y -= 2.0 * (y - offsetY);
            velY = newSpeed!(velY);
            corner = true;
        }

        if(x - offsetX < 0.0 && x + offsetX >= widthFac || self.blockWidth > widthFac) {
            x = widthFac / 2.0;
        }
        else if(x - offsetX < 0.0) {
            x = offsetX;
        }
        else if(x + offsetX >= widthFac) {
            x = widthFac - offsetX;
        }

        if(y - offsetY < 0.0 && y + offsetY >= 1.0 || self.blockHeight > 1.0) {
            y = 1.0 / 2.0;
        }
        else if(y - offsetY < 0.0) {
            y = offsetY;
        }
        else if(y + offsetY >= 1.0) {
            y = 1.0 - offsetY;
        }

        if corner && self.cornerAssist {
            let winCornerX = if(velX > 0.0) {widthFac} else {0.0};
            let winCornerY = if(velY > 0.0) {1.0} else {0.0};

            let cornerX = x + velX.signum() * offsetX;
            let cornerY = y + velY.signum() * offsetY;

            let toCorner = (winCornerX - cornerX, winCornerY - cornerY);
            let toCornerLen = (toCorner.0*toCorner.0 + toCorner.1*toCorner.1).sqrt();
            let toCornerInvLen = 1.0 / toCornerLen;
            let velMag = (velX*velX + velY*velY).sqrt();
            let unitVel = (velX / velMag, velY / velMag);

            let toBumpXIfYEdge = unitVel.0 * toCorner.1 / unitVel.1;
            let distance = if(toBumpXIfYEdge * unitVel.0.signum() < toCorner.0 * unitVel.0.signum()) {
                (toCorner.0 - toBumpXIfYEdge).abs() / widthFac
            }
            else {
                (toCorner.1 - unitVel.1 * toCorner.0 / unitVel.0).abs()
            };

            if(distance < toCornerLen * 0.1) {
                let (vx, vy) = (
                    toCorner.0 * (toCornerInvLen * velMag),
                    toCorner.1 * (toCornerInvLen * velMag)
                );

                if(self.cornerCount < 5) {
                    velX = vx;
                    velY = vy;
                    self.cornerCount = self.cornerCount + 1;
                }
                else {
                    let cosA: f32 = (3.0 / 180.0 * std::f64::consts::PI).cos() as f32;
                    let sinA: f32 = (3.0 / 180.0 * std::f64::consts::PI).cos() as f32;

                    velX = vx * cosA - vy * sinA;
                    velY = vx * sinA + vy * cosA;
                    self.cornerCount = self.cornerCount - 1;
                }
            }
            else if(self.cornerCount > 0) {
                self.cornerCount = self.cornerCount - 1;
            }
        }

        self.x = x;
        self.y = y;
        self.velX = velX;
        self.velY = velY;
    }

    fn createBuf(&self, widthFac: f32) -> [f32; 8] {
        let offsetX = self.blockWidth * 0.5;
        let offsetY = self.blockHeight * 0.5;

        let mut buf = [0.0 as f32; 8];

        macro_rules! make01 {
            ($value:expr, x) => ($value * 2.0 / widthFac - 1.0);
            ($value:expr, y) => (1.0 - $value * 2.0);
        }

        buf[0] = make01!(self.x - offsetX as f32, x);
        buf[1] = make01!(self.y - offsetY as f32, y);

        buf[2] = make01!(self.x + offsetX as f32, x);
        buf[3] = make01!(self.y - offsetY as f32, y);

        buf[4] = make01!(self.x - offsetX as f32, x);
        buf[5] = make01!(self.y + offsetY as f32, y);

        buf[6] = make01!(self.x + offsetX as f32, x);
        buf[7] = make01!(self.y + offsetY as f32, y);

        return buf;
    }
}

struct Context<'a, 'b> {
    window: &'a mut glfw::Window,
    ss1: &'b mut Screensaver,
    va: u32, vb: u32
}

fn main() {
    let mut glfw = glfw::init(glfw::FAIL_ON_ERRORS).unwrap();


    /*let (mut window, events) = glfw.with_primary_monitor(|glfw, b| {
        let pm = b.unwrap();
        let mode = pm.get_video_mode().unwrap();
        let (mut window, events) = glfw
            .create_window(mode.width, mode.height, "screensaver", glfw::WindowMode::FullScreen(pm))
            .unwrap();

        return (window, events);
    });*/

    let (mut window, events) = glfw
        .create_window(1280, 720, "screensaver", glfw::WindowMode::Windowed)
        .unwrap();

    glfw.make_context_current(Some(&mut window));

    window.set_resizable(true);
    window.set_key_polling(true);
    window.set_framebuffer_size_polling(true);

    gl::load_with(|s| glfw.get_proc_address_raw(s));

    let img = image::open("assets/image.png").unwrap().into_rgba8();

    let mut va: u32 = 0;
    let mut vb: u32 = 0;

    unsafe {
        let progId = gl::CreateProgram();

        let vert = std::fs::read_to_string("shaders/fullscreen.vert").unwrap();
        let vertId = addShaderFromString(&vert, gl::VERTEX_SHADER);
        gl::AttachShader(progId, vertId);

        let frag = std::fs::read_to_string("shaders/main.frag").unwrap();
        let fragId = addShaderFromString(&frag, gl::FRAGMENT_SHADER);
        gl::AttachShader(progId, fragId);

        gl::LinkProgram(progId);
        gl::ValidateProgram(progId);
        gl::UseProgram(progId);

        let mut imgId: u32 = 0;
        gl::GenTextures(1, &mut imgId);

        gl::ActiveTexture(gl::TEXTURE0 + 0);
        gl::BindTexture(gl::TEXTURE_2D, imgId);
        gl::TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_MAG_FILTER, gl::LINEAR as i32);
        gl::TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_MIN_FILTER, gl::LINEAR as i32);
        gl::TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_WRAP_S, gl::CLAMP_TO_BORDER as i32);
        gl::TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_WRAP_T, gl::CLAMP_TO_BORDER as i32);
        let bc = [0.0 as f32; 4];
        gl::TexParameterfv(gl::TEXTURE_2D, gl::TEXTURE_BORDER_COLOR, bc.as_ptr() as _);
        gl::TexImage2D(
            gl::TEXTURE_2D, 0, gl::RGBA8 as i32,
            img.width() as i32, img.height() as i32,
            0, gl::RGBA, gl::UNSIGNED_BYTE,
            img.as_ptr() as *const _
        );

        let loc = gl::GetUniformLocation(progId, cstr!("tex"));
        gl::Uniform1i(loc, 0);
        checkError!();

        gl::GenVertexArrays(1, &mut va);
        gl::GenBuffers(1, &mut vb);

        gl::BindVertexArray(va);
        gl::BindBuffer(gl::ARRAY_BUFFER, vb);
        gl::EnableVertexAttribArray(0);
        gl::VertexAttribPointer(0, 2, gl::FLOAT, gl::FALSE, 8, 0 as *const _);
        gl::BindVertexArray(0);

        gl::BindBuffer(gl::ARRAY_BUFFER, vb);
        gl::BufferData(gl::ARRAY_BUFFER, 8*4, 0 as _, gl::DYNAMIC_DRAW);
        gl::BindBuffer(gl::ARRAY_BUFFER, 0);
    }

    checkError!();

    let (imgWidth, imgHeight) = (img.width() as i32, img.height() as i32);

    let mut ss1 = Screensaver::new(0.25 * imgWidth as f32 / imgHeight as f32, 0.25, true);

    let mut context = Context{
        window: &mut window,
        ss1: &mut ss1,
        va: va, vb: vb
    };

    unsafe{ gl::Enable(gl::BLEND); }

    while !context.window.should_close() {
        draw(&mut context);

        glfw.poll_events();
        for (_, event) in glfw::flush_messages(&events) {
            handleEvent(&mut context, event)
        }
        checkError!();
    }
}

fn draw(context: &mut Context) {
    let (width, height) = context.window.get_size();
    let widthFac = width as f32 / height as f32;

    let drawBlock = |screensaver: &Screensaver| { unsafe {
        let buf = screensaver.createBuf(widthFac);

        gl::BindBuffer(gl::ARRAY_BUFFER, context.vb);
        gl::BufferSubData(gl::ARRAY_BUFFER, 0, 8*4, buf.as_ptr() as *const _);
        gl::BindBuffer(gl::ARRAY_BUFFER, 0);
        checkError!();

        gl::BindVertexArray(context.va);
        gl::DrawArrays(gl::TRIANGLE_STRIP, 0, 4);
        gl::BindVertexArray(0);
    } };

    unsafe{ gl::Clear(gl::COLOR_BUFFER_BIT); }

    drawBlock(context.ss1);

    glfw::Context::swap_buffers(context.window);

    context.ss1.update(widthFac);
}

fn handleEvent(context: &mut Context, event: glfw::WindowEvent) {
    match event {
        glfw::WindowEvent::Key(key, _, action, modifiers) => {
            if(key == glfw::Key::Escape) {
                context.window.set_should_close(true);
            }
            else if(key == glfw::Key::Left) {
                context.ss1.velX = context.ss1.velX.abs() * -1.0;
            }
            else if(key == glfw::Key::Right) {
                context.ss1.velX = context.ss1.velX.abs();
            }
            else if(key == glfw::Key::Up) {
                context.ss1.velY = context.ss1.velY.abs() * -1.0;
            }
            else if(key == glfw::Key::Down) {
                context.ss1.velY = context.ss1.velY.abs();
            }
        }
        glfw::WindowEvent::FramebufferSize(newWidth, newHeight) => {
            unsafe{ gl::Viewport(0, 0, newWidth, newHeight); }
        }
        _ => {}
    }
}

fn addShaderFromString(text: &String, shaderType: u32) -> u32 {
    let id;
    unsafe { id = gl::CreateShader(shaderType); };

    let bytes1 = text.as_bytes();
    let bytes2 = bytes1.as_ptr() as *const i8;
    let length = bytes1.len() as i32;

    unsafe{
        gl::ShaderSource(id, 1, &bytes2, &length);
        gl::CompileShader(id);
    }

    let mut result: i32 = 0;
    unsafe{ gl::GetShaderiv(id, gl::COMPILE_STATUS, &mut result); }
    if result == 0 {
        let mut length = 0;
        unsafe{ gl::GetShaderiv(id, gl::INFO_LOG_LENGTH, &mut length); }
        let mut msgVec = vec![0 as u8; length as usize];
        unsafe{ gl::GetShaderInfoLog(id, length, &mut length, msgVec.as_mut_ptr() as *mut i8); }
        print!("Error: ");
        std::io::Write::write(&mut std::io::stdout(), msgVec.as_slice());
    }

    return id;
}

