package com.bee.platform.datadriver.service;

import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.entity.UserInfo;
import com.bee.platform.datadriver.dto.ErpStockCheckDTO;
import com.bee.platform.datadriver.dto.ErpStockCheckProductListDTO;
import com.bee.platform.datadriver.dto.ErpStockCheckSearchDTO;
import com.bee.platform.datadriver.entity.ErpStockCheck;
import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.datadriver.rq.ErpStockCheckRQ;
import com.bee.platform.datadriver.rq.ErpStockCheckSearchRQ;

import java.util.List;

/**
 * <p>
 * 库存盘点主单表 服务类
 * </p>
 *
 * @author cheng.ke123
 * @since 2019-05-28
 */
public interface ErpStockCheckService extends IService<ErpStockCheck> {

    ResponseResult<List<ErpStockCheckSearchDTO>>  searchStockCheckByCondition(ErpStockCheckSearchRQ rq, Page page, Integer companyId);

    ErpStockCheckDTO getStockCheckById(Integer id);

    void deleteStockCheckById(AuthPlatformUserInfo userInfo, Integer id);

    void updateStockCheckState(AuthPlatformUserInfo userInfo, Integer id, Integer state);

    Integer saveStockCheck(AuthPlatformUserInfo userInfo, ErpStockCheckRQ rq);

    List<ErpStockCheckProductListDTO> getProductListByCompanyId(Integer companyId);
}
