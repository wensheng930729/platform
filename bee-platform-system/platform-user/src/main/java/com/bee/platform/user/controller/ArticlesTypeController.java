package com.bee.platform.user.controller;


import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.entity.UserInfo;
import com.bee.platform.common.utils.ConstantsUtil;
import com.bee.platform.user.authority.service.AuthPlatformUserService;
import com.bee.platform.user.dto.ArticlesTypeDTO;
import com.bee.platform.user.service.ArticlesTypeService;
import com.bee.platform.user.service.UsersService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.ObjectUtils;
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletRequest;
import java.util.List;

/**
 * <p>
 *  前端控制器
 * </p>
 *
 * @author cheng.ke123
 * @since 2019-05-07
 */
@Slf4j
@RestController
@CrossOrigin(origins = "*")
@Api(value = "articlesType", tags = "公告类型相关接口")
@RequestMapping("/articlesType")
public class ArticlesTypeController {
    /*private final static String SYS_TOKEN = "sysToken";*/
    /*@Autowired
    private UsersService usersService;*/

    @Autowired
    private ArticlesTypeService articlesTypeService;

    @Autowired
    private AuthPlatformUserService usersService;

    @ApiOperation(value = "查询所有公告类型")
    @GetMapping(value = "/getAllArticlesType")
    public ResponseResult<List<ArticlesTypeDTO>> getAllArticlesType(HttpServletRequest request){
        /*UserInfo userInfo = usersService.getUserInfo(request.getHeader(SYS_TOKEN));*/
        AuthPlatformUserInfo userInfo = usersService.getSelfInfo(request.getHeader(ConstantsUtil.SYS_TOKEN));
        if(ObjectUtils.isEmpty(userInfo)){
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,articlesTypeService.getAllArticlesType(userInfo));
    }

    @ApiOperation(value = "查询公告类型名称唯一性 名称：0存在 1不存在")
    @GetMapping(value = "/checkTypeUnique")
    public ResponseResult<Integer> checkTypeUnique(HttpServletRequest request, @RequestParam String name){
        /*UserInfo userInfo = usersService.getUserInfo(request.getHeader(SYS_TOKEN));*/
        AuthPlatformUserInfo userInfo = usersService.getSelfInfo(request.getHeader(ConstantsUtil.SYS_TOKEN));
        if(ObjectUtils.isEmpty(userInfo)){
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return articlesTypeService.checkTypeUnique(userInfo,name);
    }

}

