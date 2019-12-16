package com.bee.platform.datadriver.dao.mapper;

import com.bee.platform.datadriver.dto.ErpReceiptOrderDetailDTO;
import com.bee.platform.datadriver.entity.ErpReceiptOrderDetail;
import com.baomidou.mybatisplus.mapper.BaseMapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;

/**
 * <p>
 * 销售收款单收款详情表 Mapper 接口
 * </p>
 *
 * @author cheng.ke123
 * @since 2019-05-28
 */
public interface ErpReceiptOrderDetailMapper extends BaseMapper<ErpReceiptOrderDetail> {

    /**
     * 根据 主单id 查询详单列表
     * @param receiptOrderId  主单id
     * @return 详单列表
     */
    List<ErpReceiptOrderDetailDTO>  getDetailList(Integer receiptOrderId);

    /**
     * @Description 查询已收款订单
     * @Param ids
     * @Date 2019/6/10 17:52
     * @Author xin.huang
     * @Return
     */
    List<Integer> findOrderIdsByOrderId(@Param("ids") List<Integer> ids);

    List<ErpReceiptOrderDetailDTO> getReceiptOrderDetailByOrderId(Integer id);

}
