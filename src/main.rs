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

struct  Screensaver {
    velX: f32, velY: f32,
    x: f32, y: f32,
    lefOffset: f32, rigOffset: f32,
    topOffset: f32, botOffset: f32,
    windowWidth: f32, windowHeight: f32,
    startVelX: f32, startVelY: f32,
    cornerAssist: bool
}

impl Screensaver {
    fn new(blockWidth: f32, blockHeight: f32, windowWidth: f32, windowHeight: f32, cornerAssist: bool) -> Screensaver {
        let (lefOffset, topOffset) = (blockWidth/2.0, blockHeight/2.0);
        let (rigOffset, botOffset) = (blockWidth - lefOffset, blockHeight - topOffset);

        let (startVelX, startVelY) = ((windowWidth / 450.0).max(1.0), (windowHeight / 450.0).max(1.0));

        let (mut x, mut y) = (lefOffset as f32, topOffset as f32);
        let (mut velX, mut velY) = (startVelX.min(startVelY), startVelX.min(startVelY));

        Screensaver {
            velX: velX, velY: velY,
            x: x, y: y,
            lefOffset: lefOffset, rigOffset: rigOffset,
            topOffset: topOffset, botOffset: botOffset,
            windowWidth: windowWidth, windowHeight: windowHeight,
            startVelX: startVelX, startVelY: startVelY,
            cornerAssist: cornerAssist
        }
    }

    fn update(&mut self) {
        let mut velX = self.velX;
        let mut velY = self.velY;
        let mut x = self.x;
        let mut y = self.y;
        let lefOffset = self.lefOffset;
        let rigOffset = self.rigOffset;
        let topOffset = self.topOffset;
        let botOffset = self.botOffset;
        let width = self.windowWidth;
        let height = self.windowHeight;

        macro_rules! randFac {
            () => (0.3 +  rand::random::<f32>() * 1.1);
        }

        macro_rules! newSpeed {
            ($value:expr, $startSpeed:expr) => (lerpf($value.abs(), $startSpeed * randFac!(), 0.3));
        }

        x += velX;
        y += velY;

        let mut corner = false;

        if velX > 0.0 && x + rigOffset as f32 - width as f32 >= 0.0 {
            x -= 2.0 * (x + rigOffset as f32 - width as f32);
            velX = -newSpeed!(velX, self.startVelX);
            corner = true;
        }
        else if velX <= 0.0 && x - (lefOffset as f32) < 0.0 {
            x -= 2.0 * (x - lefOffset as f32);
            velX = newSpeed!(velX, self.startVelX);
            corner = true;
        }

        if velY > 0.0 && y + botOffset as f32 - height as f32 >= 0.0 {
            y -= 2.0 * (y + botOffset as f32 - height as f32);
            velY = -newSpeed!(velY, self.startVelY);
            corner = true;
        }
        else if velY <= 0.0 && y - (topOffset as f32) < 0.0 {
            y -= 2.0 * (y - topOffset as f32);
            velY = newSpeed!(velY, self.startVelY);
            corner = true;
        }

        if corner && self.cornerAssist {
            let winCornerX = if(velX > 0.0) {width} else {0.0};
            let winCornerY = if(velY > 0.0) {height} else {0.0};

            let cornerX = if(velX > 0.0) {x + rigOffset} else {x - lefOffset};
            let cornerY = if(velY > 0.0) {y + botOffset} else {y - topOffset};

            let toCorner = (winCornerX - cornerX, winCornerY - cornerY);
            let toCornerInvLen = (1.0 / (toCorner.0*toCorner.0 + toCorner.1*toCorner.1)).sqrt();
            let velMag = (velX*velX + velY*velY).sqrt();
            let unitVel = (velX / velMag, velY / velMag);

            let toBumpXIfYEdge = unitVel.0 * toCorner.1 / unitVel.1;
            let distance = if(toBumpXIfYEdge * unitVel.0.signum() < toCorner.0 * unitVel.0.signum()) {
                (toCorner.0 - toBumpXIfYEdge).abs() / width
            }
            else {
                (toCorner.1 - unitVel.1 * toCorner.0 / unitVel.0).abs() / height
            };

            if(distance < 0.1) {
                velX = toCorner.0 * (toCornerInvLen * velMag);
                velY = toCorner.1 * (toCornerInvLen * velMag);
            }
        }

        self.x = x;
        self.y = y;
        self.velX = velX;
        self.velY = velY;
    }

    fn createBuf(&self) -> [f32; 8] {
        let mut buf = [0.0 as f32; 8];

        macro_rules! make01 {
            ($value:expr, x) => ($value * 2.0 / self.windowWidth - 1.0);
            ($value:expr, y) => (1.0 - ($value * 2.0 / self.windowHeight as f32));
        }

        buf[0] = make01!(self.x - self.lefOffset as f32, x);
        buf[1] = make01!(self.y - self.topOffset as f32, y);

        buf[2] = make01!(self.x + self.rigOffset as f32, x);
        buf[3] = make01!(self.y - self.topOffset as f32, y);

        buf[4] = make01!(self.x - self.lefOffset as f32, x);
        buf[5] = make01!(self.y + self.botOffset as f32, y);

        buf[6] = make01!(self.x + self.rigOffset as f32, x);
        buf[7] = make01!(self.y + self.botOffset as f32, y);

        return buf;
    }
}

fn main() {
    let mut glfw = glfw::init(glfw::FAIL_ON_ERRORS).unwrap();

    let (mut window, events) = glfw
        .create_window(1280, 720, "test", glfw::WindowMode::Windowed)
        .unwrap();
    glfw.make_context_current(Some(&mut window));

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
        gl::TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_WRAP_S, gl::CLAMP_TO_EDGE as i32);
        gl::TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_WRAP_T, gl::CLAMP_TO_EDGE as i32);
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


    let (width, height) = window.get_size();
    let (imgWidth, imgHeight) = (img.width() as i32, img.height() as i32);
    let imagesInSize = 4;
    let (imageWidthCand, imageHeightCand) = (width / imagesInSize, height / imagesInSize);

    let imageWidthScaledByMaxHeight = imgWidth * imageHeightCand / imgHeight;
    let (blockWidth, blockHeight) = if imageWidthScaledByMaxHeight > imageWidthCand {
        (imageWidthCand, imgHeight * imageWidthCand / imgWidth)
    }
    else { (imageWidthScaledByMaxHeight, imageHeightCand) };

    let mut ss1 = Screensaver::new(blockWidth as f32, blockHeight as f32, width as f32, height as f32, true);
    //let mut ss2 = Screensaver::new(blockWidth as f32, blockHeight as f32, width as f32, height as f32, false);

    unsafe{ gl::Enable(gl::BLEND); }

    while !window.should_close() {
        let draw = |screensaver: &Screensaver| { unsafe {
            let buf = screensaver.createBuf();

            gl::BindBuffer(gl::ARRAY_BUFFER, vb);
            gl::BufferSubData(gl::ARRAY_BUFFER, 0, 8*4, buf.as_ptr() as *const _);
            gl::BindBuffer(gl::ARRAY_BUFFER, 0);
            checkError!();

            gl::BindVertexArray(va);
            gl::DrawArrays(gl::TRIANGLE_STRIP, 0, 4);
            gl::BindVertexArray(0);
        } };

        unsafe{ gl::Clear(gl::COLOR_BUFFER_BIT); }

        draw(&mut ss1);
        //draw(&mut ss2);

        glfw::Context::swap_buffers(&mut window);
        glfw.poll_events();
        /*for (_, event) in glfw::flush_messages(&events) {
            handleEvent(&mut window, event)
        }
        println!("after polling");*/
        checkError!();

        ss1.update();
        //ss2.update();
    }
}

fn handleEvent(window: &mut glfw::Window, event: glfw::WindowEvent) {
    match event {
        glfw::WindowEvent::Key(key, _, action, modifiers) => {

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

