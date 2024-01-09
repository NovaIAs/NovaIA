```ruby
class Question < ActiveRecord::Base
  belongs_to :user
  has_many :answers

  validates :title, presence: true
  validates :body, presence: true

  def self.search(query)
    where("title LIKE ? OR body LIKE ?", "%#{query}%", "%#{query}%")
  end
end

class Answer < ActiveRecord::Base
  belongs_to :question
  belongs_to :user

  validates :body, presence: true

  def self.best_answers(limit)
    order(votes: :desc).limit(limit)
  end
end

class User < ActiveRecord::Base
  has_many :questions
  has_many :answers

  validates :username, presence: true
  validates :email, presence: true, uniqueness: true

  def self.find_by_username(username)
    find_by(username: username)
  end
end

class Vote < ActiveRecord::Base
  belongs_to :user
  belongs_to :answer

  validates :value, presence: true, inclusion: { in: [-1, 1] }
end

class Comment < ActiveRecord::Base
  belongs_to :user
  belongs_to :question
  belongs_to :answer

  validates :body, presence: true
end

class Tag < ActiveRecord::Base
  has_many :question_tags
  has_many :questions, through: :question_tags

  validates :name, presence: true, uniqueness: true

  def self.find_by_name(name)
    find_by(name: name)
  end
end

class QuestionTag < ActiveRecord::Base
  belongs_to :question
  belongs_to :tag
end

class QuestionController < ApplicationController
  def index
    @questions = Question.all
  end

  def show
    @question = Question.find(params[:id])
  end

  def new
    @question = Question.new
  end

  def create
    @question = Question.new(question_params)

    if @question.save
      redirect_to @question, notice: 'Question created successfully.'
    else
      render :new
    end
  end

  def edit
    @question = Question.find(params[:id])
  end

  def update
    @question = Question.find(params[:id])

    if @question.update(question_params)
      redirect_to @question, notice: 'Question updated successfully.'
    else
      render :edit
    end
  end

  def destroy
    @question = Question.find(params[:id])
    @question.destroy

    redirect_to questions_path, notice: 'Question deleted successfully.'
  end

  private

  def question_params
    params.require(:question).permit(:title, :body, :tag_names)
  end
end

class AnswerController < ApplicationController
  def index
    @answers = Answer.all
  end

  def show
    @answer = Answer.find(params[:id])
  end

  def new
    @answer = Answer.new
  end

  def create
    @answer = Answer.new(answer_params)

    if @answer.save
      redirect_to @answer, notice: 'Answer created successfully.'
    else
      render :new
    end
  end

  def edit
    @answer = Answer.find(params[:id])
  end

  def update
    @answer = Answer.find(params[:id])

    if @answer.update(answer_params)
      redirect_to @answer, notice: 'Answer updated successfully.'
    else
      render :edit
    end
  end

  def destroy
    @answer = Answer.find(params[:id])
    @answer.destroy

    redirect_to answers_path, notice: 'Answer deleted successfully.'
  end

  private

  def answer_params
    params.require(:answer).permit(:body)
  end
end

class UserController < ApplicationController
  def index
    @users = User.all
  end

  def show
    @user = User.find(params[:id])
  end

  def new
    @user = User.new
  end

  def create
    @user = User.new(user_params)

    if @user.save
      redirect_to @user, notice: 'User created successfully.'
    else
      render :new
    end
  end

  def edit
    @user = User.find(params[:id])
  end

  def update
    @user = User.find(params[:id])

    if @user.update(user_params)
      redirect_to @user, notice: 'User updated successfully.'
    else
      render :edit
    end
  end

  def destroy
    @user = User.find(params[:id])
    @user.destroy

    redirect_to users_path, notice: 'User deleted successfully.'
  end

  private

  def user_params
    params.require(:user).permit(:username, :email, :password)
  end
end

class VoteController < ApplicationController
  def index
    @votes = Vote.all
  end

  def show
    @vote = Vote.find(params[:id])
  end

  def new
    @vote = Vote.new
  end

  def create
    @vote = Vote.new(vote_params)

    if @vote.save
      redirect_to @vote, notice: 'Vote created successfully.'
    else
      render :new
    end
  end

  def edit
    @vote = Vote.find(params[:id])
  end

  def update
    @vote = Vote.find(params[:id])

    if @vote.update(vote_params)
      redirect_to @vote, notice: 'Vote updated successfully.'
    else
      render :edit
    end
  end

  def destroy
    @vote = Vote.find(params[:id])
    @vote.destroy

    redirect_to votes_path, notice: 'Vote deleted successfully.'
  end

  private

  def vote_params
    params.require(:vote).permit(:value, :user_id, :answer_id)
  end
end

class CommentController < ApplicationController
  def index
    @comments = Comment.all
  end

  def show
    @comment = Comment.find(params[:id])
  end

  def new
    @comment = Comment.new
  end

  def create
    @comment = Comment.new(comment_params)

    if @comment.save
      redirect_to @comment, notice: 'Comment created successfully.'
    else
      render :new
    end
  end

  def edit
    @comment = Comment.find(params[:id])
  end

  def update
    @comment = Comment.find(params[:id])

    if @comment.update(comment_params)
      redirect_to @comment, notice: 'Comment updated successfully.'
    else
      render :edit
    end
  end

  def destroy
    @comment = Comment.find(params[:id])
    @comment.destroy

    redirect_to comments_path, notice: 'Comment deleted successfully.'
  end

  private

  def comment_params
    params.require(:comment).permit(:body, :user_id, :question_id, :answer_id)
  end
end

class TagController < ApplicationController
  def index
    @tags = Tag.all
  end

  def show
    @tag = Tag.find(params[:id])
  end

  def new
    @tag = Tag.new
  end

  def create
    @tag = Tag.new(tag_params)

    if @tag.save
      redirect_to @tag, notice: 'Tag created successfully.'
    else
      render :new
    end
  end

  def edit
    @tag = Tag.find(params[:id])
  end

  def update
    @tag = Tag.find(params[:id])

    if @tag.update(tag_params)
      redirect_to @tag, notice: 'Tag updated successfully.'
    else
      render :edit
    end
  end

  def destroy
    @tag = Tag.find(params[:id])
    @tag.destroy

    redirect_to tags_path, notice: 'Tag deleted successfully.'
  end

  private

  def tag_params
    params.require(:tag).permit(:name)
  end
end

class QuestionTagController < ApplicationController
  def index
    @question_tags = QuestionTag.all
  end

  def show
    @question_tag = QuestionTag.find(params[:id])
  end

  def new
    @question_tag = QuestionTag.new
  end

  def create
    @question_tag = QuestionTag.new(question_tag_params)