#version 460

out vec4 color;

in vec2 uv;
uniform sampler2D tex;

void main() {
    color = texture(tex, uv);
}