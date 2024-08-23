class Vec3 {
  foreign x
  foreign y
  foreign z

  foreign x=(value)
  foreign y=(value)
  foreign z=(value)

  construct new() {
    x = 0
    y = 0
    z = 0
  }

  construct new(in_x, in_y, in_z) {
    x = in_x
    y = in_y
    z = in_z
  }

  +(other) {
    x += other.x
    y += other.y
    z += other.z
  }

  -(other) {
    x -= other.x
    y -= other.y
    z -= other.z
  }

  *(other) {
    if other is Vec3 {
      x *= other.x
      y *= other.y
      z *= other.z
    } else {
      x *= other
      y *= other
      z *= other
    }
  }

  -()
}

class Entity {
  // vec3
  foreign origin
  foreign origin=(value)

  // vec3
  // pitch | yaw | roll
  foreign angles
  foreign angles=(value)

  // float
  foreign frame
  foreign frame=(value)

  // vec3
  foreign mins
  foreign maxs

  // int
  foreign modelidx

  // string
  foreign model

  // string
  foreign classname

  // float, time until next think call
  foreign nextact
  foreign nextact=(value)

  construct foreign new() 
  
  // called every frame
  think() {}

  // invoked when touched
  touch() {}

  // invoked when used
  use() {}

  // used when an object tries to move, but cant
  blocked() {}
}
