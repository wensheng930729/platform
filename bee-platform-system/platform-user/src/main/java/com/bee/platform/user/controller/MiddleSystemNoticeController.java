package com.bee.platform.user.controller;


import com.bee.platform.common.entity.*;
import com.bee.platform.common.utils.ConstantsUtil;
import com.bee.platform.user.authority.service.AuthPlatformUserService;
import com.bee.platform.user.service.MiddleSystemNoticeService;
import com.bee.platform.user.service.UsersService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.ObjectUtils;
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletRequest;

import static com.bee.platform.common.utils.ConstantsUtil.SYS_TOKEN;

/**
 * <p>
 * 中台系统通知 前端控制器
 * </p>
 *
 * @author cheng.ke123
 * @since 2019-05-09
 */
@Slf4j
@RestController
@CrossOrigin(origins = "*")
@Api(value = "middleSystemNotice", tags = "中台系统通知相关接口")
@RequestMapping("/middleSystemNotice")
public class MiddleSystemNoticeController {

    @Autowired
    private MiddleSystemNoticeService middleSystemNoticeService;
    /*@Autowired
    private UsersService usersService;*/

    @Autowired
    private AuthPlatformUserService userService;

    @ApiOperation(value = "查询中台通知信息列表",notes = "查询中台通知信息列表")
    @PostMapping("/getMiddleSystemNoticeList")
    public ResponseResult getMiddleSystemNoticeList(HttpServletRequest request,@RequestBody Integer type, Page page){
        if(ObjectUtils.isEmpty(type)){
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.PARAMETER_INCOMPLETE);
        }
        /*UserInfo userInfo = usersService.getUserInfo(request.getHeader(SYS_TOKEN));*/
        AuthPlatformUserInfo userInfo = userService.getSelfInfo(request.getHeader(ConstantsUtil.SYS_TOKEN));

        if(ObjectUtils.isEmpty(userInfo)||ObjectUtils.isEmpty(userInfo.getId())){
            log.error("获取用户信息失败");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return middleSystemNoticeService.getMiddleSystemNoticeList(userInfo,type,page);

    }

    @ApiOperation(value = "已读通知状态变更",notes = "已读通知状态变更")
    @GetMapping("/readNotice")
    public ResponseResult updateNoticeById(HttpServletRequest request,@RequestParam Long id){
        if(ObjectUtils.isEmpty(id)){
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.PARAMETER_INCOMPLETE);
        }
        /*UserInfo userInfo = usersService.getUserInfo(request.getHeader(SYS_TOKEN));*/
        AuthPlatformUserInfo userInfo = userService.getSelfInfo(request.getHeader(ConstantsUtil.SYS_TOKEN));
        if(ObjectUtils.isEmpty(userInfo)||ObjectUtils.isEmpty(userInfo.getId())){
            log.error("获取用户信息失败");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }

        return middleSystemNoticeService.updateNoticeById(userInfo,id);
    }

    @ApiOperation(value = "中台系统通知全部置为已读状态",notes = "中台系统通知全部置为已读状态")
    @GetMapping("/readAllNotice")
    public ResponseResult updateAllNotice(HttpServletRequest request){
        /*UserInfo userInfo = usersService.getUserInfo(request.getHeader(SYS_TOKEN));*/
        AuthPlatformUserInfo userInfo = userService.getSelfInfo(request.getHeader(ConstantsUtil.SYS_TOKEN));
        if(ObjectUtils.isEmpty(userInfo)||ObjectUtils.isEmpty(userInfo.getId())){
            log.error("获取用户信息失败");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return middleSystemNoticeService.updateAllNotice(userInfo);
    }




}

