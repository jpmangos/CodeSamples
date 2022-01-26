using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Mvc;
using Microsoft.Extensions.Logging;
using Assignment1.Models;
using Microsoft.AspNetCore.Http;

namespace Assignment1.Controllers
{
    public class HomeController : Controller
    {
        private Assignment1Context _context;
        public HomeController(Assignment1Context context)
        {
            _context = context;
        }
        public IActionResult Index()
        {
            return View(_context.BlogPosts.ToList());
        }
        [HttpGet]
        public IActionResult ViewBlogPost(int? id)
        {
            BlogPosts blogPost = (from b in _context.BlogPosts where b.BlogPostId == id select b).FirstOrDefault();
            List<Comments> comments = new List<Comments>();
            blogPost.User = _context.Users.Find(blogPost.UserId);
            comments = _context.Comments
                .Where(c => c.BlogPostId == blogPost.BlogPostId)
                .Select(c => c)
                .ToList();
            foreach(var comment in comments)
            {
                comment.User = _context.Users.Find(comment.UserId);
                
            }
            blogPost.Comments = comments;
            return View(blogPost);
        }
        [HttpPost]
        [HttpGet]
        public IActionResult CreateBlogPost()
        {
            return View();
        }
        [HttpPost]
        [HttpGet]
        public IActionResult EditBlogPost(int? id)
        {
            if (id == null)
            {
                return NotFound();
            }
            BlogPosts blogToDelete = (from b in _context.BlogPosts where b.BlogPostId == id select b).FirstOrDefault();
            return View(blogToDelete);
        }
        [HttpGet]
        [HttpPost]
        public IActionResult ValidateBlogPost(BlogPosts blogPost)
        {
            blogPost.UserId = Convert.ToInt32(HttpContext.Session.GetString("UserId"));
            blogPost.Posted = DateTime.Now;
            blogPost.User = (from u in _context.Users where u.UserId == blogPost.UserId select u).FirstOrDefault();
            if (ModelState.IsValid)
            {
                _context.BlogPosts.Add(blogPost);
                _context.SaveChanges();
                return RedirectToAction("Index");
            }
            else
            {
                return View("CreateBlogPost", blogPost);
            }
        }
        [HttpGet]
        [HttpPost]
        public IActionResult ModifyBlogPost(BlogPosts blogPost, int? id)
        {
            if (id == null)
            {
                return NotFound();
            }
            if (ModelState.IsValid)
            {

                BlogPosts blogToUpdate = (from m in _context.BlogPosts where m.BlogPostId == id select m).FirstOrDefault();

                _context.BlogPosts.Update(blogToUpdate);
                blogToUpdate.Title = blogPost.Title;
                blogToUpdate.Content = blogPost.Content;
                _context.SaveChanges();
            }
            return RedirectToAction("Index");
        }
        [HttpPost]
        [HttpGet]
        public IActionResult DeleteBlogPost(int? id)
        {
            BlogPosts blogToDelete = (from b in _context.BlogPosts where b.BlogPostId == id select b).FirstOrDefault();
            List<Comments> comments = new List<Comments>();
            comments = _context.Comments
                .Where(c => c.BlogPostId == blogToDelete.BlogPostId)
                .Select(c => c)
                .ToList();

            Debug.WriteLine("Comment count: " + blogToDelete.Comments.Count());
            foreach (var comment in comments) 
            {
                Debug.WriteLine("Comment ID: " + comment.CommentId);
                _context.Comments.Remove(comment);
            }
            _context.BlogPosts.Remove(blogToDelete);
            _context.SaveChanges();

            return RedirectToAction("Index");
        }
        [HttpGet]
        [HttpPost]
        public IActionResult Login()
        {
            return View();
        }
        [HttpGet]
        [HttpPost]
        public IActionResult ValidateUser(Users user)
        {
            String email = Request.Form["EmailAddress"];
            user = (from u in _context.Users where u.EmailAddress == email select u).FirstOrDefault();
            if (user == null)
            {
                ModelState.AddModelError("LoginError", "Invalid Email!");
                return View("Login", user);
            }
            else if (user.Password != Request.Form["Password"])
            {
                ModelState.AddModelError("PasswordError", "Invalid Password!");
                return View("Login", user);
            }
            else
            {
                HttpContext.Session.SetString("UserId", user.UserId.ToString());
                HttpContext.Session.SetString("RoleId", user.RoleId.ToString());
                HttpContext.Session.SetString("Name", user.FirstName + " " + user.LastName);
            }
            return RedirectToAction("Index");
        }
        public IActionResult Logout()
        {
            HttpContext.Session.Clear();
            return RedirectToAction("Index");
        }
        public IActionResult Register()
        {
            return View();
        }
        public IActionResult ValidateRegistration(Users user)
        {
            if (ModelState.IsValid)
            {
                _context.Users.Add(user);
                _context.SaveChanges();
                HttpContext.Session.SetString("UserId", user.UserId.ToString());
                HttpContext.Session.SetString("RoleId", user.RoleId.ToString());
                HttpContext.Session.SetString("Name", user.FirstName + " " + user.LastName);
                return RedirectToAction("Index");
            }
            return RedirectToAction("Login");
        }
        public IActionResult AddComment(int? id)
        {
            BlogPosts blogPost = (from b in _context.BlogPosts where b.BlogPostId == id select b).FirstOrDefault();
            Comments comment = new Comments();
            comment.Content = Request.Form["commentForm.Content"];
            comment.BlogPostId = blogPost.BlogPostId;
            
            comment.UserId = Convert.ToInt32(HttpContext.Session.GetString("UserId"));

            _context.Comments.Add(comment);
            _context.SaveChanges();

            return RedirectToAction("ViewBlogPost", new { id });
            
        }
    }
}
