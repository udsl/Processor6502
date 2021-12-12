class User(val username: String, val password: String):
  override def toString: String = s"I am user $username"


trait UserRepositoryComponent {
  val userRepository = new UserRepository

  class UserRepository {
    var users = List[User]()
    def authenticate(username: String, password: String): User = {
      println(s"authenticating user: $username, $password")
      new User(username, username)
    }

    def create(user: User): User =
      users = users.appended(user)
      user

    def delete(user: User) = println("deleting user: " + user)
  }
}

trait UserServiceComponent { this: UserRepositoryComponent =>
  val userService = new UserService
  class UserService {
    def authenticate(username: String, password: String): User =
      userRepository.authenticate(username, password)
    def create(username: String, password: String) =
      userRepository.create(new User(username, password))
    def delete(user: User) = userRepository.delete(user)
  }
}

object ComponentRegistry extends
  UserServiceComponent with UserRepositoryComponent

ComponentRegistry.userService.authenticate("Ian", "b")
