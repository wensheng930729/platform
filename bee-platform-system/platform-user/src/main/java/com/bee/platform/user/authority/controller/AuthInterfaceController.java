package com.bee.platform.user.authority.controller;


import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.user.authority.dto.AuthInterfaceDto;
import com.bee.platform.user.authority.rq.AuthInterfaceRQ;
import com.bee.platform.user.authority.rq.AuthInterfaceRoleRQ;
import com.bee.platform.user.authority.rq.AuthInterfaceSelectRQ;
import com.bee.platform.user.authority.service.AuthInterfaceService;
import com.bee.platform.user.authority.service.AuthRoleInterfaceService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import javax.validation.Valid;
import java.util.List;

/**
 * <p>
 *  前端控制器
 * </p>
 *
 * @author xin.huang
 * @since 2019-05-20
 */
@Slf4j
@RestController
@RequestMapping("/authInterface")
@CrossOrigin(origins = "*")
@Api(value = "authInterface", tags = "新权限：authInterface相关接口")
public class AuthInterfaceController {

    @Autowired
    private AuthInterfaceService authInterfaceService;

    @Autowired
    private AuthRoleInterfaceService authRoleInterfaceService;

    @ApiOperation(value="条件查询接口列表",notes="条件查询接口列表")
    @PostMapping("/list")
    public ResponseResult getList(@RequestBody AuthInterfaceSelectRQ authInterfaceSelectRQ, Page page) {
        return authInterfaceService.getList(authInterfaceSelectRQ, page);
    }

    @ApiOperation(value="添加接口",notes="添加接口")
    @PostMapping("/add")
    public ResponseResult add(@RequestBody @Valid AuthInterfaceRQ authInterfaceRQs) {
        return authInterfaceService.add(authInterfaceRQs);
    }
    
    @ApiOperation(value="保存接口",notes="根据路由、URL、请求方式添加接口")
    @PostMapping("/addByRouterUrlMethod")
    public ResponseResult<?> addByRouterUrlMethod(@RequestBody @Valid AuthInterfaceRQ authInterfaceRQs) {
    	return authInterfaceService.addByRouterUrlMethod(authInterfaceRQs);
    }

    @ApiOperation(value="更新接口",notes="更新接口")
    @ApiImplicitParam(name = "id", value = "接口id", required = true, dataType = "int")
    @PostMapping("/update/{id}")
    public ResponseResult update(@PathVariable Integer id, @RequestBody @Valid AuthInterfaceRQ authInterfaceRQs) {
        return authInterfaceService.update(id, authInterfaceRQs);
    }

    @ApiOperation(value="批量删除接口",notes="批量删除接口")
    @PostMapping("/delete/{ids}")
    public ResponseResult batchDelete(@PathVariable String ids) {
        if (StringUtils.isBlank(ids)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.INTERFACE_ID_EMPTY);
        }
        return authInterfaceService.batchDelete(ids);
    }

    @ApiOperation(value="查询角色下的接口",notes="查询角色下的接口")
    @PostMapping("/role/listInterface")
    public ResponseResult findList(Integer roleId, Page page) {
        return authRoleInterfaceService.findList(roleId, page);
    }

    @ApiOperation(value="角色下添加接口",notes="角色下添加接口")
    @PostMapping("/role/addInterface")
    public ResponseResult addRole(@RequestBody @Valid List<AuthInterfaceRoleRQ> authInterfaceRoleRQs) {
        return authRoleInterfaceService.add(authInterfaceRoleRQs);
    }

    @ApiOperation(value="更新角色下的接口",notes="更新角色下的接口")
    @PostMapping("/role/updateInterface")
    public ResponseResult updateRole(@RequestBody @Valid List<AuthInterfaceRoleRQ> authInterfaceRoleRQs) {
        return authRoleInterfaceService.update(authInterfaceRoleRQs);
    }

    @ApiOperation(value="删除角色下的接口",notes="删除角色下的接口")
    @PostMapping("/role/deleteInterface")
    public ResponseResult deleteRole(@RequestBody List<Integer> interfaceIds, Integer roleId) {
        return authRoleInterfaceService.delete(interfaceIds, roleId);
    }

    @GetMapping("/listInterfacesBySubSys")
    @ApiOperation(value = "根据子系统查询所有接口-后台")
    public ResponseResult<List<AuthInterfaceDto>> listInterfacesBySubSys(String subSys, String beeRouter){
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, authInterfaceService.listInterfacesBySubSys(subSys,beeRouter));
    }

    @ApiOperation(value="批量导入接口",notes="批量导入接口")
    @RequestMapping(value = "/excel/import/interface", method = RequestMethod.POST)
    public ResponseResult excelImport(@RequestPart MultipartFile file) {
        try {
           return  authInterfaceService.excelImport(file.getInputStream());
        } catch (Exception e) {
            log.error("excel批量导入接口异常:{}",e);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.IMPORT_INTERFACE_FAIL);
    }

    @GetMapping("/getInterfaceDetail")
    @ApiOperation(value = "获取接口详情")
    public ResponseResult<AuthInterfaceDto> getResourceDetail(@RequestParam String id){
        if (org.springframework.util.StringUtils.isEmpty(id)) {
            log.info("参数错误");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        return authInterfaceService.getInterfaceDetail(id);
    }
}

