 package com.bee.platform.user.controller;

 import com.baomidou.mybatisplus.mapper.EntityWrapper;
 import com.baomidou.mybatisplus.plugins.pagination.Pagination;
 import com.bee.platform.business.rq.NewsInfoRQ;
 import com.bee.platform.common.dto.NewsParam;
 import com.bee.platform.common.entity.*;
 import com.bee.platform.common.utils.ConstantsUtil;
 import com.bee.platform.common.utils.PageUtils;
 import com.bee.platform.common.utils.UserInfoUtils;
 import com.bee.platform.common.utils.WebUtils;
 import com.bee.platform.user.dto.NewDTO;
 import com.bee.platform.user.dto.NewsDTO;
 import com.bee.platform.user.entity.News;
 import com.bee.platform.user.entity.NewsType;
 import com.bee.platform.user.entity.ReponseMessage;
 import com.bee.platform.user.service.ManageUserService;
 import com.bee.platform.user.service.NewsService;
 import com.bee.platform.user.service.UsersService;
 import io.swagger.annotations.Api;
 import io.swagger.annotations.ApiOperation;
 import lombok.extern.slf4j.Slf4j;
 import org.apache.commons.lang3.StringUtils;
 import org.springframework.beans.BeanUtils;
 import org.springframework.beans.factory.annotation.Autowired;
 import org.springframework.util.ObjectUtils;
 import org.springframework.web.bind.annotation.*;

 import javax.servlet.http.HttpServletRequest;
 import javax.validation.Valid;
 import java.util.List;

 /**
  * 资讯接口类
 * @author Raphael.dq
 * @date 2019/03/04
 */
 @Api(value = "资讯API", tags = "资讯相关API")
 @RestController
 @CrossOrigin(origins = "*")
 @Slf4j
 @RequestMapping("/")
public class NewsController {
     
     @Autowired
     NewsService newsService;
     
     @Autowired
     private UsersService usersService;

     @Autowired
     private ManageUserService manageUserService;

     @Autowired
     private UserInfoUtils userInfoUtils;
     
     @ApiOperation(value = "获取资讯列表")
     @RequestMapping(value = "/api/user/news",method = RequestMethod.GET)
     public Object newses(Page page) {
         EntityWrapper<News> wrapper = new EntityWrapper<>();
         wrapper.orderBy("create_at", false);
         return newsService.listByPage(new com.baomidou.mybatisplus.plugins.Page<>(page.getCurrentPage(), page.getPageSize()), wrapper);
     }
     
     @ApiOperation(value = "**********获取资讯总数") 
     @RequestMapping(value = "/api/user/news/count",method = RequestMethod.GET)
     public Object count() {
         return newsService.countAll();
     }
     
     
     /**---------------------8-新增------------------**/
     @ApiOperation(value = "*******条件查询已发布资讯列表")
     @RequestMapping(value = "/api/user/news/query",method = RequestMethod.GET)
     public Object query(Page page, NewsDTO newDTO) {
         News news = new News();
         BeanUtils.copyProperties(newDTO, news);
         EntityWrapper<News> wrapper = new EntityWrapper<News>(news);
         if (StringUtils.isNotBlank(newDTO.getStartTime()) && StringUtils.isNotBlank(newDTO.getEndTime())) {
             wrapper.between("create_at", newDTO.getStartTime() + " 00:00:00", newDTO.getEndTime() + " 23:59:59");
         }
         //未删除
         wrapper.eq("state", 0);
         wrapper.orderBy("create_at", false);
         Pagination pagination = PageUtils.transFromPage(page);
         return newsService.listByPage(pagination, wrapper);
     }
     
     @ApiOperation(value = "**********获取一篇资讯") 
     @RequestMapping(value = "/api/user/news/{id}",method = RequestMethod.GET)
     public Object hitNews(@PathVariable("id") int id) {
         return newsService.hitNews(id);
     }
     
     

     /**------------------------------9--复用------------------------------------**/
     @ApiOperation(value = "**********获取一篇资讯") 
     @RequestMapping(value = "/api/user/news/inner/{id}",method = RequestMethod.GET)
     public Object getInnerOneNews(@PathVariable("id") int id) {
         return newsService.getInnerOneNews(id);
     }

     
     
     @ApiOperation("根据类型获取资讯")
     @GetMapping("/api/user/news/category")
     public ResponseResult<NewDTO> getAllByType(@RequestParam(value = "page", defaultValue = "0") Integer page,
                                                @RequestParam(value = "size", defaultValue = "5") Integer size, @RequestParam Integer type) {
         if(type==null){
             ReponseMessage message=new ReponseMessage();
             message.setCode(0);
             message.setMessage("文章类型不能为空");
         }
         News news = new News();
         news.setType(type);
         //查询未删除的资讯
         news.setState(0);
         EntityWrapper<News> wrapper = new EntityWrapper<>(news);
         wrapper.orderBy("create_at", false);
         return newsService.listByPageable(new com.baomidou.mybatisplus.plugins.pagination.Pagination(page,size), wrapper);
     }

     @ApiOperation(value = "*****发表资讯")
     @RequestMapping(value = "/manager/news/publish", method = RequestMethod.POST)
     public ResponseResult addNews(HttpServletRequest request, @RequestBody @Valid NewsInfoRQ newsInfoRQ) {
         AuthPlatformUserInfo userInfo = userInfoUtils.getSimpleUserInfo(request);
         ResponseResult result;
         try {
             result = newsService.addNews(userInfo.getUsername(), newsInfoRQ);
         } catch (Exception e) {
             e.printStackTrace();
             result = ResponseResult.buildResponseResult(ResCodeEnum.FAILED);
         }
         return result;
     }

     /**---------------------------------------10-----复用旧接口，增加修改type的字段,PUT变成POST-------------------------**/
     @ApiOperation(value = "*********修改资讯")
     @RequestMapping(value = "/manager/news/update/{id}", method = RequestMethod.POST)
     public Object updateNews(@RequestBody @Valid NewsInfoRQ newsInfoRQ, @PathVariable("id") int id) {
         ResponseResult result;
         try {
             newsInfoRQ.setId(id);
             result = newsService.updateNews(newsInfoRQ);
         } catch (Exception e) {
             e.printStackTrace();
             result = ResponseResult.buildResponseResult(ResCodeEnum.FAILED);
         }
         return result;
     }

     /**----------------------------------------12-----新增，批量删除资讯-------------------------------------**/
     @ApiOperation(value = "************删除资讯")
     @RequestMapping(value = "/manager/news/delete", method = RequestMethod.POST)
     public Object deleteNews(@RequestBody NewsParam param) {
         if (ObjectUtils.isEmpty(param)) {
             return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
         }
         ResponseResult result = newsService.delete(param.getNewIds());
         return result;
     }

     @ApiOperation(value = "删除资讯")
     @DeleteMapping("/manager/news/delete/{id}")
     public Object deleteNews(@PathVariable int id) {
         return newsService.deleteNews(id);
     }

     @ApiOperation(value = "查询资讯类型")
     @GetMapping("/manager/news/queryTypes")
     public ResponseResult<List<NewsType>> queryNewsTypes(){
        return newsService.queryNewsTypes();
     }

     @ApiOperation(value = "新增或者发表资讯")
     @PostMapping(value = "/manager/news/publish_or_update")
     public ResponseResult addOrUpdateNews(@RequestHeader("sysToken") String sysToken,HttpServletRequest request, @RequestBody @Valid NewsInfoRQ newsInfoRQ) {
         AuthPlatformUserInfo userInfo = userInfoUtils.getSimpleUserInfo(request);
         ResponseResult result;
         try {
             result = newsService.addOrUpdateNews(userInfo.getId(), newsInfoRQ);
         } catch (Exception e) {
             log.error("新增或者发表资讯失败 ！",e);
             result = ResponseResult.buildResponseResult(ResCodeEnum.FAILED);
         }
         return result;
     }
}
