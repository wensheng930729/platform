package com.bee.platform.common.service.impl;

import com.bee.platform.common.constants.enums.EnumCommon;
import com.bee.platform.common.dao.mapper.SequenceMapper;
import com.bee.platform.common.entity.Sequence;
import com.bee.platform.common.service.GenerateIdService;
import com.bee.platform.common.utils.ConstInfos;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.text.DecimalFormat;
import java.text.SimpleDateFormat;
import java.util.Date;

/**
 * @ClassName GenerateIdServiceImpl
 * @Description 序列号相关处理
 * @author qhwang
 * @version 1.0.0
 * @Date 2019/4/28 14:01
 */
@Service
public class GenerateIdServiceImpl implements GenerateIdService {

    @Autowired
    private SequenceMapper sequenceMapper;

    /**
     * 获取工单编号
     * @return
     */
    @Override
    public String getWorkOrderNumber() {
        //工单编号 5位数流水号（11开头）+日期（如190318）+4位随机数（1234）
        StringBuffer workOrderNumber = new StringBuffer();
        //获取序列号
        workOrderNumber.append(getSeq(ConstInfos.Sequence.workOrdersSeq.getKey()));
        //时间信息：年2位+月2位+日2位
        SimpleDateFormat formatter = new SimpleDateFormat("yyMMdd");
        workOrderNumber.append(formatter.format(new Date()));
        //4位随机数
        workOrderNumber.append(String.valueOf((int)(Math.random() * 10000)));

        return workOrderNumber.toString();
    }

    /**
     * 根据序列名获取序列值
     * @param sequenceKey
     * @return
     */
    @Transactional(rollbackFor = Exception.class)
    public String getSeq(String sequenceKey){
        String defaultSequenceValue;
        Sequence sequence = sequenceMapper.selectOne(new Sequence().setSequenceKey(sequenceKey).setStatus(EnumCommon.IsActive.is_active.getKey()));
        //获取序列号
        Integer intiSequenceValue = sequence.getSequenceValue();
        try {
            //转换成字符串
            String sequenceValue = String.valueOf(intiSequenceValue);
            //高位自动补0
            if (sequenceValue.length() < 5) {
                DecimalFormat df = new DecimalFormat("00000");
                defaultSequenceValue = df.format(Integer.parseInt(sequenceValue));
            } else {
                //超过5位，截取5位
                defaultSequenceValue = sequenceValue.substring(sequenceValue.length() - 5);
            }
        } catch (Exception e) {
            //报错后，随机生成一个5位序列
            defaultSequenceValue = String.valueOf((int)(Math.random() * 100000));
        }finally {
            //执行修改序列号操作
            sequence.setSequenceValue(intiSequenceValue +1);
            sequenceMapper.updateById(sequence);
        }
        return defaultSequenceValue;
    }
}
