package com.bee.platform.datadriver.service;

import com.bee.platform.datadriver.entity.ErpCode;
import com.baomidou.mybatisplus.service.IService;

import java.util.List;

/**
 * <p>
 * 码表 服务类
 * </p>
 *
 * @author cheng.ke123
 * @since 2019-05-31
 */
public interface ErpCodeService extends IService<ErpCode> {

    /**
     * 查询erp码表
     * @param code
     * @return
     */
    List<ErpCode> listErpCode(String code);
}
