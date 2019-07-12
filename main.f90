program main
  implicit none

  integer, parameter :: d = 1  ! Number of inputs
  integer, parameter :: k = 1  ! Number of outputs
  integer, parameter :: h = 8  ! Number of hidden nodes
  integer, parameter :: n = 64  ! Training data counts
  integer, parameter :: epochs = 1000  ! Iteration of training
  real(8), parameter :: learn_rate = 0.03

  type type_weights
    real(8) :: w1(h, d), w2(k, h), b1(h), b2(k)
  end type type_weights

  call training()

  contains

  subroutine training()
    ! Main subroutine
    integer :: itr, i
    real(8) :: train_x(d, n), train_y(k, n)
    real(8) :: loss, y(k), diff(k)
    type(type_weights) :: weights
    call get_initial_weights(weights)
    call get_training_data(train_x, train_y)
    do itr = 1, epochs
      loss = 0.0
      do i = 1, n  ! Todo: shuffled order
        y = forward(train_x(:, i), weights)
        diff = y - train_y(:, i)
        loss = loss + sum(diff ** 2)
        call backward(train_x(:, i), weights, diff)
      end do
      print *, loss  ! Decays to zero
    end do
  end subroutine training

  pure function activation(x)
    ! Hyperbolic tangent activation function
    real(8), intent(in) :: x(h)
    real(8) :: activation(h)
    activation = tanh(x)
  end function activation

  pure function activation_prime(x)
    ! Derivative of the activation function
    real(8), intent(in) :: x(h)
    real(8) :: activation_prime(h)
    activation_prime = cosh(x) ** (-2)
  end function activation_prime

  pure function forward(x, w)
    ! Forward evaluation of neural network
    real(8), intent(in) :: x(d)
    type(type_weights), intent(in) :: w
    real(8) :: forward(k)
    forward = matmul(w%w2, activation(matmul(w%w1, x) + w%b1)) + w%b2
  end function forward

  subroutine backward(x, w, diff)
    ! Calculate derivative of loss function in weight space and gradient descent
    real(8), intent(in) :: x(d), diff(k)
    type(type_weights), intent(inout) :: w
    real(8) :: v1(h), v2(h)
    v1 = matmul(diff, w%w2) * activation_prime(matmul(w%w1, x) + w%b1)
    v2 = activation(matmul(w%w1, x) + w%b1)
    w%w1 = w%w1 - learn_rate * matmul(reshape(v1, (/h, 1/)), reshape(x, (/1, d/)))
    w%b1 = w%b1 - learn_rate * v1
    w%w2 = w%w2 - learn_rate * matmul(reshape(diff, (/k, 1/)), reshape(v2, (/1, h/)))
    w%b2 = w%b2 - learn_rate * diff
  end subroutine backward

  subroutine get_training_data(train_x, train_y)
    ! Training data is y = 2 x^2 - 1
    real(8), intent(out) :: train_x(d, n), train_y(k, n)
    integer :: i
    if ((d /= 1) .or. (k /= 1)) then
      print *, "Shape mismatch. Stop."
      stop 3
    end if
    do i = 1, n
      train_x(1, i) = -1.0 + (2.0 * i / n)
      train_y(1, i) = 2 * train_x(1, i) ** 2 - 1
    end do
  end subroutine get_training_data

  subroutine get_initial_weights(res)
    ! Randomly initialize weights
    type(type_weights), intent(out) :: res
    call random_number(res%w1)
    call random_number(res%w2)
    call random_number(res%b1)
    call random_number(res%b2)
    res%w1 = res%w1 - 0.5
    res%w2 = res%w2 - 0.5
    res%b1 = res%b1 - 0.5
    res%b2 = res%b2 - 0.5
  end subroutine get_initial_weights
end program main

