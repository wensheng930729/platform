package com.bee.platform.costcontrol.controller;


import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.utils.ConstantsUtil;
import com.bee.platform.common.utils.UserInfoUtils;
import com.bee.platform.common.utils.WebUtils;
import com.bee.platform.costcontrol.service.ErpCostAllocationService;
import com.bee.platform.costcontroller.dto.ErpCostAllocationBoxDTO;
import com.bee.platform.costcontroller.dto.ErpCostAllocationDetailDTO;
import com.bee.platform.costcontroller.dto.ErpCostAllocationQueryDTO;
import com.bee.platform.costcontroller.rq.ErpCostAllocationAddRQ;
import com.bee.platform.costcontroller.rq.ErpCostAllocationQueryRQ;
import com.bee.platform.costcontroller.rq.ErpCostAllocationSwitchRQ;
import com.bee.platform.costcontroller.rq.ErpCostAllocationUpdateRQ;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.ObjectUtils;
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletRequest;
import java.util.List;

/**
 * <p>
 * erp成本小工具-成本配置 前端控制器
 * </p>
 *
 * @author liliang123
 * @since 2019-06-24
 */
@RestController
@RequestMapping("/erpCostAllocation")
@Api(value = "ErpCostAllocationController", tags = "erp-成本配置相关接口")
public class ErpCostAllocationController {

    @Autowired
    private ErpCostAllocationService costAllocationService;
    @Autowired
    private UserInfoUtils userInfoUtils;

    @PostMapping("/add")
    @ApiOperation(value = "新增")
    public ResponseResult addCostAllocation(HttpServletRequest request, @RequestBody ErpCostAllocationAddRQ rq) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getSimpleUserInfo(request);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        return costAllocationService.addCostAllocation(rq, userInfo);
    }

    @PostMapping("/update")
    @ApiOperation(value = "更新")
    public ResponseResult updateCostAllocation(HttpServletRequest request, @RequestBody ErpCostAllocationUpdateRQ rq) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getSimpleUserInfo(request);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        return costAllocationService.updateCostAllocation(rq);
    }

    @GetMapping("/delete/{id}")
    @ApiOperation(value = "删除")
    public ResponseResult deleteCostAllocation(HttpServletRequest request, @PathVariable("id") Integer id) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getSimpleUserInfo(request);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        return costAllocationService.deleteCostAllocation(id);
    }

    @PostMapping("/switch")
    @ApiOperation(value = "禁用启用")
    public ResponseResult updateStatus(HttpServletRequest request, @RequestBody ErpCostAllocationSwitchRQ rq) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getSimpleUserInfo(request);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        return costAllocationService.updateStatus(rq);
    }

    @PostMapping("/list")
    @ApiOperation(value = "列表")
    public ResponseResult<List<ErpCostAllocationQueryDTO>> getCostList(HttpServletRequest request, @RequestBody ErpCostAllocationQueryRQ rq, Page page) {
        Integer companyId = Integer.valueOf(WebUtils.getParam(ConstantsUtil.COMPANY, request));
        if (ObjectUtils.isEmpty(companyId) && ObjectUtils.isEmpty(rq.getCompanyId())) {
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        AuthPlatformUserInfo userInfo = userInfoUtils.getSimpleUserInfo(request);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        return costAllocationService.getCostList(companyId,rq, userInfo, page);
    }

    @GetMapping("/detail/{id}")
    @ApiOperation(value = "详情")
    public ResponseResult<ErpCostAllocationDetailDTO> getCostAllocationDetail(HttpServletRequest request, @PathVariable("id") Integer id) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getSimpleUserInfo(request);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        return costAllocationService.getCostAllocationDetail(id);
    }

    @GetMapping("/boxList")
    @ApiOperation(value = "下拉列表-企业及子企业")
    public ResponseResult<List<ErpCostAllocationBoxDTO>> getCostAllocationBox(HttpServletRequest request) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getSimpleUserInfo(request);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        return costAllocationService.getCostAllocationBox(userInfo);
    }

    @GetMapping("/boxListByCompany/{companyId}")
    @ApiOperation(value = "下拉列表-企业")
    public ResponseResult<List<ErpCostAllocationBoxDTO>> getCostAllocationBoxByCompanyId(HttpServletRequest request, @PathVariable("companyId") Integer companyId) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getSimpleUserInfo(request);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        return costAllocationService.getCostAllocationBoxByCompanyId(companyId);
    }
}

