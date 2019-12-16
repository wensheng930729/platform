package com.bee.platform.datadriver.controller;


import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.utils.UserInfoUtils;
import com.bee.platform.datadriver.dto.ErpStockDetailDTO;
import com.bee.platform.datadriver.dto.ErpStockSearchListDTO;
import com.bee.platform.datadriver.rq.ErpStockDetailListRQ;
import com.bee.platform.datadriver.rq.ErpStockSearchRQ;
import com.bee.platform.datadriver.service.ErpStockService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.ObjectUtils;
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletRequest;
import javax.validation.Valid;
import java.util.List;

/**
 * <p>
 * 库存表 前端控制器
 * </p>
 *
 * @author chenjie123123
 * @since 2019-06-03
 */

@Slf4j
@RestController
@CrossOrigin(origins = "*")
@Api(value = "erpStock", tags = "库存表相关接口")
@RequestMapping("/erpStock")
public class ErpStockController {

    @Autowired
    private ErpStockService erpStockService;

    @Autowired
    private UserInfoUtils userInfoUtils;


    @ApiOperation(value = "条件搜索现存明细")
    @PostMapping("/searchStockByCondition")
    public ResponseResult<List<ErpStockSearchListDTO>> searchStockByCondition(HttpServletRequest request, @RequestBody ErpStockSearchRQ rq, Page page) {
        if (ObjectUtils.isEmpty(rq)) {
            log.info("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }

        AuthPlatformUserInfo userInfo = userInfoUtils.getSimpleUserInfo(request);
        if (ObjectUtils.isEmpty(userInfo) || ObjectUtils.isEmpty(userInfo.getOrgId())) {
            log.info("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        Integer companyId = userInfo.getOrgId();

        if (ObjectUtils.isEmpty(companyId) && ObjectUtils.isEmpty(rq.getCompanyId())) {
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }

        return erpStockService.searchStockByCondition(rq, page, companyId);
    }

    @ApiOperation(value = "查询现存明细详情")
    @PostMapping("/getStockDetailByCondition")
    public ResponseResult<List<ErpStockDetailDTO>> getStockDetailByCondition(@RequestBody @Valid ErpStockDetailListRQ rq, Page page) {
        if (ObjectUtils.isEmpty(rq)) {
            log.info("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }

        return erpStockService.getStockDetailByCondition(rq, page);
    }

}

