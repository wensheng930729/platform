package com.bee.platform.datadriver.controller;


import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.datadriver.dto.ErpPurchaseStmtDetailDTO;
import com.bee.platform.datadriver.entity.ErpPurchaseStmtDetail;
import com.bee.platform.datadriver.service.ErpPurchaseStmtDetailService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.StringUtils;
import org.springframework.web.bind.annotation.*;
import javax.servlet.http.HttpServletRequest;
import java.util.List;

/**
 * <p>
 * 采购结算单结算详情 前端控制器
 * </p>
 *
 * @author jie.chen123
 * @since 2019-05-27
 */
@Slf4j
@RestController
@CrossOrigin(origins = "*")
@Api(value = "erpPurchaseStmtDetail", tags = "采购结算单结算详情相关接口")
@RequestMapping("/erpPurchaseStmtDetail")
public class ErpPurchaseStmtDetailController {

    @Autowired
    private ErpPurchaseStmtDetailService erpPurchaseStmtDetailService;

    @GetMapping("/listPurchaseStmtDetailByOrder")
    @ApiOperation(value = "根据订单id查询采购结算单列表")
    public ResponseResult<List<ErpPurchaseStmtDetailDTO>> listPurchaseStmtDetailByOrder(HttpServletRequest request, @RequestParam String id){
        if (StringUtils.isEmpty(id)) {
            log.info("参数错误");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,erpPurchaseStmtDetailService.listPurchaseStmtDetailByOrder(id));
    }

}

