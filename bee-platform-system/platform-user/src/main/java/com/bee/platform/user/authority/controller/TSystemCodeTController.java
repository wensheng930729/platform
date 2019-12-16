package com.bee.platform.user.authority.controller;


import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.utils.ConstantsUtil;
import com.bee.platform.common.utils.PageUtils;
import com.bee.platform.user.authority.dto.SystemCodeDTO;
import com.bee.platform.user.authority.rq.SubSystemRQ;
import com.bee.platform.user.authority.rq.SystemCodeQueryRQ;
import com.bee.platform.user.authority.service.AuthPlatformUserService;
import com.bee.platform.user.authority.service.TSystemCodeTService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.ObjectUtils;
import org.springframework.util.StringUtils;
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletRequest;
import java.util.List;

/**
 * <p>
 * 系统码表 前端控制器
 * </p>
 *
 * @author chenjie123123
 * @since 2019-05-27
 */
@Slf4j
@RestController
@CrossOrigin(origins = "*")
@Api(value = "authRole", tags = "新权限：子系统配置接口")
@RequestMapping("/tSystemCodeT")
public class TSystemCodeTController {

    @Autowired
    private AuthPlatformUserService userService;
    @Autowired
    private TSystemCodeTService systemCodeTService;

    @GetMapping("/listSubSystem")
    @ApiOperation(value = "获取子系统列表-后台")
    public ResponseResult<List<SystemCodeDTO>> listSubSystem(SystemCodeQueryRQ rq, Page page){
        Pagination pagination = PageUtils.transFromPage(page);
        List<SystemCodeDTO> list = systemCodeTService.listSubSystem(rq,pagination);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,list,PageUtils.transToPage(pagination));
    }

    @PostMapping("/addSubSystem")
    @ApiOperation(value = "新增子系统-后台")
    public ResponseResult<ResCodeEnum> addSubSystem(HttpServletRequest request, @RequestBody SubSystemRQ rq){
        AuthPlatformUserInfo userInfo = userService.getSelfInfo(request.getHeader(ConstantsUtil.SYS_TOKEN));
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        if (ObjectUtils.isEmpty(rq)) {
            log.info("参数错误");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        return systemCodeTService.addSubSystem(userInfo,rq);
    }

    @GetMapping("/deleteSubSystem")
    @ApiOperation(value = "删除子系统-后台")
    public ResponseResult<ResCodeEnum> deleteSubSystem(HttpServletRequest request, @RequestParam String id){
        AuthPlatformUserInfo userInfo = userService.getSelfInfo(request.getHeader(ConstantsUtil.SYS_TOKEN));
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        if (StringUtils.isEmpty(id)) {
            log.info("参数错误");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        return systemCodeTService.deleteSubSystem(userInfo,id);
    }

}

