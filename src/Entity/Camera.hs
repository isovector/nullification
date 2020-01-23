module Entity.Camera where


cameraProto :: Entity
cameraProto = newEntity
  { eVel      = Just $ V2 0 0
  , eSpeed    = Just 800
  , eIsCamera = Just ()
  }

