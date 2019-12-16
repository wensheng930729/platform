package com.bee.platform.datadriver.service.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.bee.platform.datadriver.entity.ErpCode;
import com.bee.platform.datadriver.dao.mapper.ErpCodeMapper;
import com.bee.platform.datadriver.service.ErpCodeService;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;

/**
 * <p>
 * 码表 服务实现类
 * </p>
 *
 * @author cheng.ke123
 * @since 2019-05-31
 */
@Service
public class ErpCodeServiceImpl extends ServiceImpl<ErpCodeMapper, ErpCode> implements ErpCodeService {

    @Autowired
    private ErpCodeMapper erpCodeMapper;

    /**
     * 查询erp码表
     * @param code
     * @return
     */
    @Override
    public List<ErpCode> listErpCode(String code) {
        return erpCodeMapper.selectList(new EntityWrapper<>(new ErpCode().setCode(code)));
    }
}
