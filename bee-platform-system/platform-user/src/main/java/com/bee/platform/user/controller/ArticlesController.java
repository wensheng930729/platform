package com.bee.platform.user.controller;


import com.bee.platform.business.dto.ArticleTypesDTO;
import com.bee.platform.business.dto.ArticlesDTO;
import com.bee.platform.business.rq.ArticleAddRQ;
import com.bee.platform.business.rq.ArticleSearchRQ;
import com.bee.platform.common.entity.*;
import com.bee.platform.common.utils.ConstantsUtil;
import com.bee.platform.common.utils.WebUtils;
import com.bee.platform.user.authority.service.AuthPlatformUserService;
import com.bee.platform.user.service.ArticlesService;
import com.bee.platform.user.service.UsersService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.ObjectUtils;
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletRequest;
import java.util.List;
import java.util.Map;

import static com.bee.platform.common.utils.ConstantsUtil.SYS_TOKEN;

/**
 * <p>
 *  前端控制器
 * </p>
 *
 * @author jie.zhang123
 * @since 2019-03-04
 */
@Slf4j
@RestController
@CrossOrigin(origins = "*")
@Api(value = "articles", tags = "公告相关接口")
@RequestMapping("/api/articles")
public class ArticlesController {

    @Autowired
    private ArticlesService articleService;
    /*@Autowired
    private UsersService usersService;*/
    @Autowired
    private AuthPlatformUserService usersService;

    @ApiOperation(value = "获取文章列表")
    @RequestMapping(value = "/getLatest", method = RequestMethod.GET)
    public ResponseResult< Map<String,Object>> getLastedArticles(HttpServletRequest request, @RequestParam(value = "page", defaultValue = "0") Integer page,
                                                                @RequestParam(value = "size", defaultValue = "5") Integer size) {
        /*UserInfo userInfo = usersService.getUserInfo(request.getHeader(SYS_TOKEN));*/
        AuthPlatformUserInfo userInfo = usersService.getSelfInfo(request.getHeader(ConstantsUtil.SYS_TOKEN));
        if(ObjectUtils.isEmpty(userInfo)){
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return articleService.getLastedArticles(userInfo, new Page(size,page));
    }

    @ApiOperation(value = "获取文章列表")
    @RequestMapping(method = RequestMethod.GET)
    public ResponseResult<List<ArticlesDTO>> getAllByAdmin(HttpServletRequest request, Integer type,Page page) {
        /*UserInfo userInfo = usersService.getUserInfo(WebUtils.getParam(ConstantsUtil.SYS_TOKEN,request));*/
        AuthPlatformUserInfo userInfo = usersService.getSelfInfo(request.getHeader(ConstantsUtil.SYS_TOKEN));
        if(ObjectUtils.isEmpty(userInfo)){
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        if(ObjectUtils.isEmpty(type)){
            return ResponseResult.buildResponseResult(ResCodeEnum.PARAMETER_INCOMPLETE);
        }
        return articleService.getAllByAdmin(userInfo, type,page);
    }

//    @ApiOperation(value = "条件查询公告列表")
//    @RequestMapping(value = "/getArticleList", method = RequestMethod.POST)
//    public ResponseResult<List<ArticlesDTO>> getArticleList(ArticleSearchRQ rq, HttpServletRequest request,Page page){
//        UserInfo userInfo = usersService.getUserInfo(WebUtils.getParam(ConstantsUtil.SYS_TOKEN,request));
//        if(ObjectUtils.isEmpty(userInfo) || ObjectUtils.isEmpty(rq)){
//            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
//        }
//        return articleService.getArticleList(userInfo,rq,page);
//    }

    @ApiOperation(value = "条件查询公告列表")
    @RequestMapping(value = "/getArticlesByCondition", method = RequestMethod.POST)
    public ResponseResult<List<ArticlesDTO>> getArticlesByCondition(HttpServletRequest request, @RequestBody ArticleSearchRQ rq, Page page){
        if(ObjectUtils.isEmpty(rq)){
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.PARAMETER_INCOMPLETE);
        }
        /*UserInfo userInfo = usersService.getUserInfo(WebUtils.getParam(ConstantsUtil.SYS_TOKEN,request));*/
        AuthPlatformUserInfo userInfo = usersService.getSelfInfo(request.getHeader(ConstantsUtil.SYS_TOKEN));
        if(ObjectUtils.isEmpty(userInfo) || ObjectUtils.isEmpty(rq)){
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return articleService.getArticlesByCondition(userInfo,rq,page);
    }

    @ApiOperation(value = "根据id获取公告详情")
    @GetMapping(value = "/getArticleById")
    public ResponseResult<ArticlesDTO> getArticle(HttpServletRequest request, @RequestParam Integer id){
        if(ObjectUtils.isEmpty(id)){
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.PARAMETER_INCOMPLETE);
        }
        /*UserInfo userInfo = usersService.getUserInfo(request.getHeader(SYS_TOKEN));*/
        AuthPlatformUserInfo userInfo = usersService.getSelfInfo(request.getHeader(ConstantsUtil.SYS_TOKEN));
        if(ObjectUtils.isEmpty(userInfo)){
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return articleService.getArticle(userInfo,id);
    }

    @ApiOperation(value = "删除公告")
    @PostMapping(value = "/deleteArticle")
    public ResponseResult deleteArticle(HttpServletRequest request, @RequestParam Integer id) {
        /*UserInfo userInfo = usersService.getUserInfo(request.getHeader(SYS_TOKEN));*/
        AuthPlatformUserInfo userInfo = usersService.getSelfInfo(request.getHeader(ConstantsUtil.SYS_TOKEN));
        return articleService.deleteArticle(userInfo,id);
    }

    @ApiOperation(value = "发布 修改公告")
    @PostMapping(value = "/editArticle")
    public ResponseResult<ResCodeEnum> addArticle(HttpServletRequest request, @RequestBody ArticleAddRQ rq) {
        /*UserInfo userInfo = usersService.getUserInfo(request.getHeader(SYS_TOKEN));*/
        AuthPlatformUserInfo userInfo = usersService.getSelfInfo(request.getHeader(ConstantsUtil.SYS_TOKEN));
        if(ObjectUtils.isEmpty(userInfo)){
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return articleService.editArticle(userInfo, rq);
    }

    @ApiOperation(value = "获取公告类型列表")
    @GetMapping(value = "/getArticleTypes")
    public ResponseResult<List<ArticleTypesDTO>> getArticleTypes(HttpServletRequest request, Page page, String name) {
        /*UserInfo userInfo = usersService.getUserInfo(WebUtils.getParam(ConstantsUtil.SYS_TOKEN,request));*/
        AuthPlatformUserInfo userInfo = usersService.getSelfInfo(request.getHeader(ConstantsUtil.SYS_TOKEN));
        if(ObjectUtils.isEmpty(userInfo)){
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return articleService.getArticleTypes(userInfo, name,page);
    }

    @ApiOperation(value = "删除公告类型")
    @PostMapping(value = "/deleteArticleType")
    public ResponseResult deleteArticleType(HttpServletRequest request, @RequestParam int id) {
        /*UserInfo userInfo = usersService.getUserInfo(request.getHeader(SYS_TOKEN));*/
        AuthPlatformUserInfo userInfo = usersService.getSelfInfo(request.getHeader(ConstantsUtil.SYS_TOKEN));
        return articleService.deleteArticleType(userInfo,id);
    }

    @ApiOperation(value = "新增 编辑公告类型")
    @PostMapping(value = "/editArticleType")
    public ResponseResult editArticleType(HttpServletRequest request, Integer id, @RequestParam String name) {
        /*UserInfo userInfo = usersService.getUserInfo(request.getHeader(SYS_TOKEN));*/
        AuthPlatformUserInfo userInfo = usersService.getSelfInfo(request.getHeader(ConstantsUtil.SYS_TOKEN));
        if(ObjectUtils.isEmpty(userInfo)){
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return articleService.modifyArticleType(userInfo, id,name);
    }
}

