package com.bee.platform.datadriver.controller;


import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.utils.ConstantsUtil;
import com.bee.platform.common.utils.PageUtils;
import com.bee.platform.common.utils.UserInfoUtils;
import com.bee.platform.common.utils.WebUtils;
import com.bee.platform.datadriver.dto.ErpFurnaceBoxDTO;
import com.bee.platform.datadriver.dto.ErpFurnaceListDTO;
import com.bee.platform.datadriver.dto.ErpFurnaceOneDTO;
import com.bee.platform.datadriver.enums.EnumBusinessType;
import com.bee.platform.datadriver.rq.ErpFurnacaAddRQ;
import com.bee.platform.datadriver.rq.ErpFurnacaUpdateRQ;
import com.bee.platform.datadriver.service.ErpFurnaceService;
import com.bee.platform.datadriver.support.OperateType;
import com.bee.platform.datadriver.support.annotation.Log;
import com.bee.platform.user.service.feign.AuthUserFeignClient;
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
 * 炉子档案 前端控制器
 * </p>
 *
 * @author jie.chen123
 * @since 2019-05-27
 */
@Slf4j
@RestController
@RequestMapping("/erpFurnace")
@Api(value = "ErpFurnaceController", tags = "erp设备-炉子相关接口")
public class ErpFurnaceController {

    @Autowired
    private AuthUserFeignClient userInfoFeignClient;
    @Autowired
    private ErpFurnaceService furnaceService;
    @Autowired
    private UserInfoUtils userInfoUtils;

    @PostMapping("/add")
    @ApiOperation(value = "新增erp炉子信息")
    @Log(businessType = EnumBusinessType.FURNACE, operateType = OperateType.ADD)
    public ResponseResult<Integer> addFurnace(HttpServletRequest request, @RequestBody ErpFurnacaAddRQ rq) {
        String sysToken = request.getHeader("sysToken");
        AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(sysToken).getObject();
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        return furnaceService.addFurnace(rq, userInfo);
    }

    @GetMapping("/delete/{id}")
    @ApiOperation(value = "删除erp炉子信息")
    @Log(businessType = EnumBusinessType.FURNACE, operateType = OperateType.DELETE)
    public ResponseResult<Integer> deleteFurnace(HttpServletRequest request, @PathVariable("id") Integer id) {
        String sysToken = request.getHeader("sysToken");
        AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(sysToken).getObject();
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        return furnaceService.deleteFurnace(id, userInfo);
    }

    @PostMapping("/update")
    @ApiOperation(value = "更新erp炉子信息")
    @Log(businessType = EnumBusinessType.FURNACE, operateType = OperateType.EDIT)
    public ResponseResult<Integer> updateFurnace(HttpServletRequest request, @RequestBody ErpFurnacaUpdateRQ rq) {
        String sysToken = request.getHeader("sysToken");
        AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(sysToken).getObject();
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        return furnaceService.updateFurnace(rq, userInfo);
    }

    @GetMapping("/updateStatus/{id}/{status}")
    @ApiOperation(value = "炉子启用--禁用")
    @Log(businessType = EnumBusinessType.FURNACE, operateType = OperateType.EDIT)
    public ResponseResult<Integer> updateFurnaceStatus(HttpServletRequest request, @PathVariable("id") Integer id, @PathVariable("status") Integer status) {
        String sysToken = request.getHeader("sysToken");
        AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(sysToken).getObject();
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        return furnaceService.updateFurnaceStatus(id, status, userInfo);
    }


    @GetMapping("/{id}")
    @ApiOperation(value = "根据id查询炉子信息")
    public ResponseResult<ErpFurnaceOneDTO> getById(@PathVariable("id") Integer id) {
        return furnaceService.getById(id);
    }

    @GetMapping("/query")
    @ApiOperation(value = "查询erp炉子档案列表")
    public ResponseResult<List<ErpFurnaceListDTO>> query(HttpServletRequest request, Page page) {
        String sysToken = request.getHeader("sysToken");
        Pagination pagination = PageUtils.transFromPage(page);
        AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(sysToken).getObject();
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        Integer companyId = Integer.valueOf(WebUtils.getParam(ConstantsUtil.COMPANY, request));
        if (ObjectUtils.isEmpty(companyId)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NO_ENTERPRISE_ID);
        }
        return furnaceService.query(companyId, pagination);
    }

    @GetMapping("/query/num")
    @ApiOperation(value = "查询当前用户企业下的炉号-下拉列表")
    public ResponseResult<List<ErpFurnaceBoxDTO>> queryFurnaceNum(HttpServletRequest request) {
        String sysToken = request.getHeader("sysToken");
        AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(sysToken).getObject();
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        return furnaceService.queryFurnaceNum(userInfo);
    }


    @ApiOperation(value = "查询当前登录用户及其子企业的炉子信息")
    @GetMapping("/getFurnaceList")
    public ResponseResult<List<ErpFurnaceBoxDTO>> getFurnaceList(HttpServletRequest request) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getSimpleUserInfo(request);
        if (ObjectUtils.isEmpty(userInfo) || ObjectUtils.isEmpty(userInfo.getOrgId())) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        String sysToken = request.getHeader("sysToken");
        List<ErpFurnaceBoxDTO> dto = furnaceService.getFurnaceList(userInfo, sysToken);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto);
    }


}

