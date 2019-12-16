package com.bee.platform.user.controller;


import com.bee.platform.business.dto.PostListDTO;
import com.bee.platform.business.rq.PostAddRQ;
import com.bee.platform.common.entity.*;
import com.bee.platform.common.utils.UserInfoUtils;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.user.authority.service.AuthPlatformUserService;
import com.bee.platform.user.entity.ZPost;
import com.bee.platform.user.service.ZPostService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletRequest;
import java.util.List;


/**
 * <p>
 * 前端控制器
 * </p>
 *
 * @author hongchuan.he123
 * @since 2019-04-25
 */
@Slf4j
@RestController
@CrossOrigin(origins = "*")
@Api(value = "post", tags = "职位相关接口")
@RequestMapping("/api/post")
public class ZPostController {

    private final static String SYS_TOKEN = "sysToken";

    @Autowired
    private AuthPlatformUserService usersService;
    @Autowired
    private ZPostService postService;

    @Autowired
    private UserInfoUtils userInfoUtils;


    @ApiOperation(value = "条件查询职位列表")
    @GetMapping(value = "/getPostList")
    public ResponseResult<List<PostListDTO>> getPostList(HttpServletRequest request, String name, Integer departmentId, Page page) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getSimpleUserInfo(request);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return postService.getPostList(userInfo, name, departmentId, page);
    }

    @ApiOperation(value = "根据id查询职位信息")
    @GetMapping("/getPostById")
    public ResponseResult<PostListDTO> getPostById(@RequestParam Integer id){
        if(ObjectUtils.isEmpty(id)){
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);

        }
        PostListDTO dto =  postService.getPostById(id);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,dto);
    }




    @ApiOperation(value = "编辑 新增职位", notes = "编辑 新增职位,企业管理员有此权限")
    @PostMapping(value = "/editPost")
    public ResponseResult editPost(HttpServletRequest request, @RequestBody PostAddRQ rq) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getSimpleUserInfo(request);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        if(ObjectUtils.isEmpty(rq)){
            return ResponseResult.buildResponseResult(ResCodeEnum.PARAMETER_INCOMPLETE);
        }
        return postService.editPost(userInfo, rq);
    }

    @PostMapping(value = "/deletePost")
    @ApiOperation(value = "删除职位）", notes = "删除职位")
    public ResponseResult deletePost(HttpServletRequest request, @RequestParam() Integer id) {
        if (ObjectUtils.isEmpty(id)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.PARAMETER_INCOMPLETE);
        }
        //用户验证
        AuthPlatformUserInfo userInfo = userInfoUtils.getSimpleUserInfo(request);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return postService.deletePost(userInfo, id);
    }

    @ApiOperation(value = "职位验证名称唯一性 0存在 1不存在", notes = "职位验证名称唯一性")
    @RequestMapping(value = "/postNameCheck", method = RequestMethod.GET)
    public ResponseResult<Integer> postNameCheck(HttpServletRequest request, @RequestParam String name, @RequestParam Integer departmentId) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getSimpleUserInfo(request);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return postService.postNameCheck(userInfo, name, departmentId);
    }

    /**
     * 根据部门id查询 部门下的职位
     *
     * @param departmentId
     * @return
     */
    @ApiOperation(value = "根据部门id查询 部门下的职位", notes = "根据部门id查询 部门下的职位")
    @RequestMapping(value = "/department", method = RequestMethod.GET)
    ResponseResult<List<ZPost>> getByDepartment(Integer departmentId) {
        return postService.getByDepartment(departmentId);
    }
}

