package com.bee.platform.user.controller;


import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.common.entity.*;
import com.bee.platform.common.utils.PageUtils;
import com.bee.platform.common.utils.UserInfoUtils;
import com.bee.platform.user.dto.SystemNoticeDTO;
import com.bee.platform.user.service.ManageUserService;
import com.bee.platform.user.service.SystemNoticeService;
import com.bee.platform.user.vo.SystemNoticeVO;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletRequest;
import javax.validation.Valid;
import java.util.List;

/**
 *  后台管理系统通知相关接口
 *
 * @author junyang.li
 * @since 2019-05-06
 */
@Slf4j
@RestController
@CrossOrigin(origins = "*")
@RequestMapping("/systemNotice")
@Api(value = "后台管理——系统通知相关接口", tags = "后台管理——系统通知相关接口")
public class SystemNoticeController {

    @Autowired
    private ManageUserService manageUserService;

    @Autowired
    private SystemNoticeService systemNoticeService;


    @Autowired
    private UserInfoUtils userInfoUtils;

    @GetMapping("/getNoticeList")
    @ApiOperation(value = "系统通知列表查询", notes = "系统通知列表查询")
    @ApiImplicitParam(value = "通知类型",name = "type",defaultValue = "0 未读 1 已读 空全部")
    public ResponseResult<List<SystemNoticeDTO>> getNoticeList(@RequestHeader("sysToken")String sysToken, HttpServletRequest request,Integer type, Page page) {

        AuthPlatformUserInfo userInfo = userInfoUtils.getSimpleUserInfo(request);
        Pagination pagination= PageUtils.transFromPage(page);
        return systemNoticeService.getNoticeList(userInfo.getId(),type,pagination);
    }

    @PostMapping("/readNotices")
    @ApiOperation(value = "系统通知全部已读", notes = "系统通知全部已读")
    public ResponseResult<ResCodeEnum> updateNotices(@RequestHeader("sysToken")String sysToken,HttpServletRequest request,@RequestBody @Valid SystemNoticeVO vo) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getSimpleUserInfo(request);
        return systemNoticeService.updateNotices(userInfo.getId(),vo.getNoticeIds());
    }

}

