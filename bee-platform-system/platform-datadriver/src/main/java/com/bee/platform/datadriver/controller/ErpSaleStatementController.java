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
import com.bee.platform.datadriver.dto.ErpSaleStatementDTO;
import com.bee.platform.datadriver.dto.ErpSaleStatementDetailDTO;
import com.bee.platform.datadriver.enums.EnumBusinessType;
import com.bee.platform.datadriver.rq.ErpSaleStatementSelectRQ;
import com.bee.platform.datadriver.rq.SaleStatementRQ;
import com.bee.platform.datadriver.service.ErpSaleStatementService;
import com.bee.platform.datadriver.support.OperateType;
import com.bee.platform.datadriver.support.annotation.Log;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.ObjectUtils;
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletRequest;
import javax.validation.Valid;
import java.util.List;
import java.util.Objects;

/**
 * <p>
 * 销售结算单 前端控制器
 * </p>
 *
 * @author jie.chen123
 * @since 2019-05-27
 */
@Slf4j
@RestController
@CrossOrigin(origins = "*")
@RequestMapping("/erpSaleStatement")
@Api(value = "销售结算操作相关接口", tags = "销售结算操作相关接口")
public class ErpSaleStatementController {

    @Autowired
    private ErpSaleStatementService erpSaleStatementService;
    @Autowired
    private UserInfoUtils userInfoUtils;

    @ApiOperation(value = "新增销售结算", notes = "新增销售结算")
    @Log(businessType= EnumBusinessType.SALE_STATEMENT, operateType = OperateType.ADD)
    @PostMapping("/add/saleStatement")
    public ResponseResult<Integer> addSaleStatement(HttpServletRequest request, @RequestBody() @Valid SaleStatementRQ saleStatementRQ) {
        AuthPlatformUserInfo simpleUserInfo = userInfoUtils.getSimpleUserInfo(request);
        if (ObjectUtils.isEmpty(simpleUserInfo)) {
            log.info("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return erpSaleStatementService.add(simpleUserInfo, saleStatementRQ);
    }

    @ApiOperation(value = "更新销售结算", notes = "更新销售结算")
    @Log(businessType= EnumBusinessType.SALE_STATEMENT, operateType = OperateType.EDIT)
    @PostMapping("/update/saleStatement")
    public ResponseResult<Integer> updateSaleStatement(HttpServletRequest request, @RequestBody() @Valid SaleStatementRQ saleStatementRQ) {
        AuthPlatformUserInfo simpleUserInfo = userInfoUtils.getSimpleUserInfo(request);
        if (ObjectUtils.isEmpty(simpleUserInfo)) {
            log.info("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return erpSaleStatementService.update(simpleUserInfo, saleStatementRQ);
    }

    @ApiOperation(value = "删除销售结算", notes = "删除销售结算")
    @Log(businessType= EnumBusinessType.SALE_STATEMENT, operateType = OperateType.DELETE)
    @PostMapping("/delete/saleStatement")
    public ResponseResult<Integer> deleteSaleStatement(Integer id) {
        return erpSaleStatementService.deleteSaleStatement(id);
    }

    @ApiOperation(value = "查询销售结算详情", notes = "查询销售结算详情")
    @ApiImplicitParam(name = "id", value = "销售结算单id: 列表中的statementId", required = true)
    @GetMapping("/info/saleStatement")
    public ResponseResult<ErpSaleStatementDTO> findStatementOrder(Integer id) {
        return erpSaleStatementService.findStatementOrder(id);
    }

    @ApiOperation(value = "条件查询销售结算单列表", notes = "条件查询销售结算单列表")
    @PostMapping("/list/saleStatement")
    public ResponseResult<List<ErpSaleStatementDetailDTO>> findSaleStatementOrderList(HttpServletRequest request, @RequestBody() @Valid ErpSaleStatementSelectRQ saleStatementSelectRQ, Page page) {
        Integer companyId = Integer.valueOf(WebUtils.getParam(ConstantsUtil.COMPANY, request));
        if (Objects.isNull(companyId) && Objects.isNull(saleStatementSelectRQ.getCompanyId())){
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        Pagination pagination = PageUtils.transFromPage(page);
        AuthPlatformUserInfo simpleUserInfo = userInfoUtils.getSimpleUserInfo(request);
        if (ObjectUtils.isEmpty(simpleUserInfo)) {
            log.info("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        List<ErpSaleStatementDetailDTO> saleStatementOrderList = erpSaleStatementService.findSaleStatementOrderList(companyId, saleStatementSelectRQ, pagination);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, saleStatementOrderList, PageUtils.transToPage(pagination));
    }

    @ApiOperation(value = "更新销售结算单状态", notes = "更新销售结算单状态")
    @ApiImplicitParams({@ApiImplicitParam(name = "id", value = "销售结算单id", required = true),
            @ApiImplicitParam(name = "state", value = "状态 0以保存 1已确认", required = true)})
    @Log(businessType= EnumBusinessType.SALE_STATEMENT, operateType = OperateType.EDIT)
    @PostMapping("/updateState/saleStatement")
    public ResponseResult<Integer> updateState(HttpServletRequest request, Integer id, Integer state) {
        AuthPlatformUserInfo simpleUserInfo = userInfoUtils.getSimpleUserInfo(request);
        if (ObjectUtils.isEmpty(simpleUserInfo)) {
            log.info("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return erpSaleStatementService.updateState(simpleUserInfo, id, state);
    }

    @ApiOperation(value = "根据销售订单id查看销售结算情况")
    @GetMapping("/getSaleStatementInfo/{id}")
    public ResponseResult<List<ErpSaleStatementDetailDTO>> getSaleStatementInfo(@PathVariable  Integer id){
        if (ObjectUtils.isEmpty(id)) {
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        return erpSaleStatementService.getSaleStatementInfo(id);
    }
}

