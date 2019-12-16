package com.bee.platform.dinas.datadriver.service;

import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.dinas.datadriver.dto.*;
import com.bee.platform.dinas.datadriver.entity.DinasInspectionGoods;
import com.bee.platform.dinas.datadriver.rq.*;

import java.util.List;

/**
 * <p>
 * 验货磅单表 服务类
 * </p>
 *
 * @author liliang123
 * @since 2019-08-13
 */
public interface DinasInspectionGoodsService extends IService<DinasInspectionGoods> {

    ResponseResult<List<DinasInspectionGoodsSearchDTO>> searchInspectionGoodsByCondition(DinasInspectionGoodsSearchRQ rq, Page page, Integer companyId);

    Integer saveInspectionGoods(AuthPlatformUserInfo userInfo, DinasInspectionGoodsSaveRQ rq);

    Integer updateInspectionGoods(AuthPlatformUserInfo userInfo, DinasInspectionGoodsUpdateRQ rq);

    DinasInspectionGoodsDTO getInspectionGoodsById(Integer id);

    List<Integer> deleteInspectionGoodsByIds(AuthPlatformUserInfo userInfo, List<Integer> ids);

    List<DinasPurchaseCodeListDTO> getPurchaseCodeList(Integer companyId);

    List<DinasSaleCodeListDTO> getSaleCodeList(Integer companyId);

    List<DinasProductListDTO> getProductListByPurchaseIdAndSaleId(DinasGetProductListRQ rq);

    List<DinasProductSpecListDTO> getProductSpecListByPurchaseIdAndSaleIdAndProductId(DinasGetProductSpecListRQ rq);
}
