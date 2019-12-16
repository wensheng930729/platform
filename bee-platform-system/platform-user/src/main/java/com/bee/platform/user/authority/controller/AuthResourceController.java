package com.bee.platform.user.authority.controller;


import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.utils.ConstantsUtil;
import com.bee.platform.common.utils.PageUtils;
import com.bee.platform.user.authority.dto.AuthResourceDetailDTO;
import com.bee.platform.user.authority.rq.AuthResourceQueryRQ;
import com.bee.platform.user.authority.rq.AuthResourceRQ;
import com.bee.platform.user.authority.service.AuthPlatformUserService;
import com.bee.platform.user.authority.service.AuthResourceService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.ObjectUtils;
import org.springframework.util.StringUtils;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import javax.servlet.http.HttpServletRequest;
import javax.validation.Valid;
import java.util.List;

/**
 * <p>
 * 资源表 前端控制器
 * </p>
 *
 * @author jie.chen123
 * @since 2019-05-20
 */

@Slf4j
@RestController
@CrossOrigin(origins = "*")
@Api(value = "authResource", tags = "新权限：资源相关接口")
@RequestMapping("/authResource")
public class AuthResourceController {

    @Autowired
    private AuthResourceService resourceService;
    @Autowired
    private AuthPlatformUserService userService;
    @Autowired
    private AuthPlatformUserService authPlatformUserService;

    @PostMapping("/resourcesWithPage")
    @ApiOperation(value = "获得所有资源-后台（分页）")
    public ResponseResult<List<AuthResourceDetailDTO>> listResourcesWithPage(@RequestBody AuthResourceQueryRQ rq, Page page){
        if (StringUtils.isEmpty(rq)) {
            log.info("参数错误");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        Pagination pagination = PageUtils.transFromPage(page);
        List<AuthResourceDetailDTO> list = resourceService.listResourcesWithPage(rq,pagination);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, list, PageUtils.transToPage(pagination));
    }

    @PostMapping("/addResource")
    @ApiOperation(value = "添加资源")
    public ResponseResult<ResCodeEnum> addResource(HttpServletRequest request, @RequestBody @Valid AuthResourceRQ resourceRQ){
        AuthPlatformUserInfo userInfo = userService.getSelfInfo(request.getHeader(ConstantsUtil.SYS_TOKEN));
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        return resourceService.addResource(userInfo, resourceRQ);
    }

    @GetMapping("/deleteResource")
    @ApiOperation(value = "删除资源")
    public ResponseResult<ResCodeEnum> deleteResource(HttpServletRequest request, @RequestParam String id){
        AuthPlatformUserInfo userInfo = userService.getSelfInfo(request.getHeader(ConstantsUtil.SYS_TOKEN));
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        if (StringUtils.isEmpty(id)) {
            log.info("参数错误");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        return resourceService.deleteResource(userInfo, id);
    }

    @PostMapping("/updateResource")
    @ApiOperation(value = "修改资源")
    public ResponseResult<ResCodeEnum> updateResource(HttpServletRequest request, @RequestBody @Valid AuthResourceRQ resourceRQ){
        AuthPlatformUserInfo userInfo = userService.getSelfInfo(request.getHeader(ConstantsUtil.SYS_TOKEN));
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        return resourceService.updateResource(userInfo, resourceRQ);
    }

    @GetMapping("/listResourcesBySubSys")
    @ApiOperation(value = "根据子系统查询所有资源-后台")
    public ResponseResult<List<AuthResourceDetailDTO>> listResourcesBySubSys(HttpServletRequest request,String subSys){
        AuthPlatformUserInfo userInfo = userService.getSelfInfo(request.getHeader(ConstantsUtil.SYS_TOKEN));
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, resourceService.listResourcesBySubSys(userInfo,subSys));
    }

    @GetMapping("/resourcesByUser")
    @ApiOperation(value = "获得所有资源-用户")
    public ResponseResult<List<AuthResourceDetailDTO>> listResourcesByUser(HttpServletRequest request, 
        @RequestParam String subSys, @RequestParam(required = false) String sysToken){
        if (org.apache.commons.lang3.StringUtils.isEmpty(sysToken)) {
            sysToken = request.getHeader(ConstantsUtil.SYS_TOKEN);
        }
        AuthPlatformUserInfo userInfo = userService.getSelfInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        if (StringUtils.isEmpty(subSys)) {
            log.info("参数错误");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, resourceService.listResourcesByUser(userInfo,subSys));
    }

    @GetMapping("/resourcesByBackUser")
    @ApiOperation(value = "获得所有资源-后台用户")
    public ResponseResult<List<AuthResourceDetailDTO>> resourcesByBackUser(HttpServletRequest request){
        AuthPlatformUserInfo userInfo = userService.getSelfInfo(request.getHeader(ConstantsUtil.SYS_TOKEN));
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, resourceService.resourcesByBackUser(userInfo));
    }

    @GetMapping("/getUserResourceByRoleId")
    @ApiOperation(value = "根据用户角色获取用户菜单")
    public ResponseResult<List<AuthResourceDetailDTO>> getUserResourceByRoleId(@RequestParam String subSys,@RequestParam Integer roleId, @RequestParam String sysToken){
        AuthPlatformUserInfo userInfo = userService.getSelfInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        if (StringUtils.isEmpty(subSys)) {
            log.info("参数错误");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, resourceService.getUserResourceByRoleId(subSys,roleId,userInfo));
    }

    @GetMapping("/resourcesByUserSubSys")
    @ApiOperation(value = "获得所有资源-用户")
    public ResponseResult<List<AuthResourceDetailDTO>> listResourcesByUser(
            @RequestParam String subSys, @RequestParam(required = false) String sysToken){
        AuthPlatformUserInfo userInfo = userService.getSelfInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        if (StringUtils.isEmpty(subSys)) {
            log.info("参数错误");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, resourceService.listResourcesByUser(userInfo,subSys));
    }
    
    @ApiOperation(value="批量导入接口",notes="批量导入接口")
    @RequestMapping(value = "/excel/import/resource", method = RequestMethod.POST)
    public ResponseResult excelImport(@RequestPart MultipartFile file) {
        try {
            return  resourceService.excelImport(file.getInputStream());
        } catch (Exception e) {
            log.error("excel批量导入接口异常:{}",e);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.IMPORT_INTERFACE_FAIL);
    }

    @GetMapping("/getResourceDetail")
    @ApiOperation(value = "获取资源详情")
    public ResponseResult<AuthResourceDetailDTO> getResourceDetail(HttpServletRequest request, @RequestParam String id){
        if (StringUtils.isEmpty(id)) {
            log.info("参数错误");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        return resourceService.getResourceDetail(id);
    }

    @GetMapping("/listSubResourceBackNoPage")
    @ApiOperation(value = "获取下一级资源列表-不分页")
    public ResponseResult<List<AuthResourceDetailDTO>> listSubResourceBackNoPage(HttpServletRequest request, @RequestParam String id){
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,resourceService.listSubResourceBackNoPage(id));
    }


}

